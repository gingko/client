const { app } = require("electron");
const { promisify } = require("util");
const fs = require("fs-extra");
const path = require("path");
const execFile = promisify(require("child_process").execFile);
const { path7za } = require("7zip-bin");
const _ = require("lodash");
const firstline = require("firstline");
const crypto = require("crypto");
const moment = require("moment");
const Store = require("electron-store");
const FileType = require("file-type");
const GingkoError = require("../shared/errors");

const PouchDB = require("pouchdb");

/* ============================================================================
 * EXPOSED FUNCTIONS
 * ============================================================================
 */

/**
 * newSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 * - Open a new swap folder (if it doesn't exist)
 * Return the new swapFolderPath if successful.
 *
 * @param {String} swapFolderPath
 *
 * @returns {Promise<String, Error>} new swapFolderPath if successful
 */
async function newSwapFolder(swapFolderPath) {
  await swapFolderCheck(swapFolderPath);
  await fs.ensureDir(path.join(swapFolderPath, "leveldb"));
  new Store({ name: "meta", cwd: swapFolderPath, defaults: { version: 1 } });

  return swapFolderPath;
}

/**
 * openFile : String -> Promise String Error
 *
 * Given filepath
 * - Verify the file type
 * - Open a new swap folder (if it doesn't exist)
 * - Make a backup of the original file
 * - Add filepath to swap.json
 *
 * @param {String} filepath
 *
 * @returns {Promise<String, Error>} the new swapFolderPath if successful.
 */
async function openFile(filepath) {
  const fileFormat = await determineFiletype(filepath);

  switch (fileFormat.format) {
    case false:
      throw new Error("Not a valid .gko file\nPossibly using legacy format.");

    case JSON_FILE:
      const originalStats = await fs.stat(filepath);
      await makeBackup(filepath);
      //await swapFolderCheck(swapFolderPath);
      return { filepath: filepath, data: fileFormat.data, lastSaved : originalStats.mtimeMs }  ;

    case GKO: {
      const swapName = fullpathFilename(filepath);
      const swapFolderPath = path.join(app.getPath("userData"), swapName);

      await makeBackup(filepath);
      await swapFolderCheck(swapFolderPath);
      await extractFile(filepath, swapFolderPath);
      addFilepathToSwap(filepath, swapFolderPath);
      return swapFolderPath;
    }
    case LEGACY_GKO: {
      throw new Error("Not a valid .gko file\nPossibly using legacy file format. If you need to access this file, contact support.");
    }
  }
}

/**
 * saveSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 * - If swap.json doesn't exist, then it's Untitled
 * - Get targetPath from swap.json
 * - Zip folder to backupPath
 * - Copy backupPath to targetPath, with overwrite
 *
 * @param {String} swapFolderPath
 *
 * @returns {Promise<String, Error>} targetPath if successful
 *
 */
async function saveSwapFolder(swapFolderPath) {
  const savedBefore = await fs.pathExists(
    path.join(swapFolderPath, "swap.json")
  );

  if (savedBefore) {
    const targetPath = getFilepathFromSwap(swapFolderPath);
    const backupPath = getBackupPath(targetPath, Date.now());
    await zipFolder(swapFolderPath, backupPath);
    await fs.copy(backupPath, targetPath, { overwrite: true });

    return targetPath;
  } else {
    // TODO: Beware of this
    const targetPath = undefined;
    // Never-saved/Untitled document
    const backupPath = getBackupPath(null, Date.now());
    await zipFolder(swapFolderPath, backupPath);
    return targetPath;
  }
}

/**
 * saveSwapFolderAs : String -> String -> Promise String Error
 *
 * Given swapFolderPath and newFilepath
 * - Move swap folder to new swap folder
 * - Delete original swap folder
 * - Zip folder to backupPath
 * - Copy backupPath to newFilepath, with overwrite
 *
 * @param {String} originalSwapFolderPath
 * @param {String} newTargetPath
 *
 * @returns {Promise<String, Error>} newSwapFolderPath if successful
 */
async function saveSwapFolderAs(originalSwapFolderPath, newTargetPath) {
  const newSwapFolderPath = await swapCopy(
    originalSwapFolderPath,
    newTargetPath
  );

  const backupPath = getBackupPath(newTargetPath, Date.now());
  await deleteSwapFolder(originalSwapFolderPath);
  await zipFolder(newSwapFolderPath, backupPath);
  await fs.copy(backupPath, newTargetPath, { overwrite: true });

  return newSwapFolderPath;
}

/**
 *  backupSwapFolder : String -> Promise String Error
 *
 * Given a swapFolderPath
 * - Get backupPath
 * - Zip folder to backupPath
 *
 * @param {String} swapFolderPath
 *
 * @returns {Promise<String, Error>} backupPath if successful
 */
async function backupSwapFolder(swapFolderPath) {
  const targetPath = getFilepathFromSwap(swapFolderPath);
  const backupPath = getBackupPath(targetPath, Date.now());

  await zipFolder(swapFolderPath, backupPath);

  return backupPath;
}

/**
 * saveLegacyFolderAs : String -> String -> String -> Promise String Error
 *
 * Given legacyFolderPath, legacyName, and newTargetPath
 * - Copy swap folder to new swap folder
 * - Nest new swap folder into 'leveldb' path
 * - Add meta.json with file format version
 * - Zip folder to backupPath
 * - Copy backupPath to newFilepath, with overwrite
 *
 * @param {String} legacyFolderPath
 * @param {String} newTargetPath
 *
 * @returns {Promise<String, Error>} newSwapFolderPath if successful
 */
async function saveLegacyFolderAs(legacyFolderPath, newTargetPath) {
  const newSwapFolderPath = await swapCopy(
    legacyFolderPath,
    newTargetPath,
    true
  );
  new Store({
    name: "meta",
    cwd: newSwapFolderPath,
    defaults: { version: 1 }
  });
  const backupPath = getBackupPath(newTargetPath, Date.now());

  await zipFolder(newSwapFolderPath, backupPath);
  await fs.copy(backupPath, newTargetPath);

  return newSwapFolderPath;
}

/**
 * deleteSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 *  - Verify that it is inside userData folder
 *  - Verify that it contains "leveldb" subdirectory
 *  - Verify that it contains "swap.json" file
 *  - Verify that it contains "meta.json" file
 * If so, make a backup, then delete it.
 *
 * @param {String} swapFolderPath
 *
 * @returns {Promise<String, Error>} swapFolderPath if successful
 */
async function deleteSwapFolder(swapFolderPath) {
  try {
    const relative = path.relative(app.getPath("userData"), swapFolderPath);

    // Determine if a path is subdirectory of another in Node.js
    // https://stackoverflow.com/a/45242825/43769
    if (
      !(!!relative && !relative.startsWith("..") && !path.isAbsolute(relative))
    ) {
      return;
    }

    const filesExist = await Promise.all([
      fs.pathExists(path.join(swapFolderPath, "leveldb")),
      fs.pathExists(path.join(swapFolderPath, "swap.json")),
      fs.pathExists(path.join(swapFolderPath, "meta.json"))
    ]);

    if (filesExist.some(fileExists => !fileExists)) {
      return;
    }

    await backupSwapFolder(swapFolderPath);
    await fs.remove(swapFolderPath);
    return swapFolderPath;
  } catch (err) {
    throw new Error(
      "Could not delete swap folder.\n" + swapFolderPath + "\n" + err.message
    );
  }
}

/**
 * destroyDb : String -> Promise void
 *
 * Destroys a db by a given name
 *
 * @param {String} dbName
 *
 * @returns {Promise<void>}
 */
async function destroyDb(dbName) {
  const dbPath = path.join(app.getPath("userData"), dbName);

  try {
    await fs.unlink(
      path.join(app.getPath("userData"), `window-state-${dbName}.json`)
    );
  } finally {
    return new PouchDB(dbPath).destroy();
  }
}

/**
 * getHashWithoutStartTime : String -> Promise String Error
 *
 * @param {String} filepath
 *
 * @returns {Promise<String, Error>} the base64-encoded sha1 hash for the contents of the file, with it's `start_time` attribute set to empty.
 *  Throws an error if no file is found at the specified filepath.
 */
async function getHashWithoutStartTime(filepath) {
  const hash = crypto.createHash("sha1");
  const filecontents = await fs.readFile(filepath, "utf8");
  const transformedContents = filecontents.replace(
    /"start_time":".*","db_info"/,
    "\"start_time\":\"\",\"db_info\""
  );

  hash.update(transformedContents);

  return hash.digest("base64");
}


async function truncateBackups() {
  let dir = path.join(app.getPath("userData"), "backups");
  const exists = await fs.pathExists(dir);

  if (exists) {
    let files = await fs.readdir(dir);
    let sortedFiltered = files.filter((f)=> {return f.endsWith(".gko"); }).sort().reverse();
    let grouped = _.groupBy(sortedFiltered, (f) => { return f.slice(0,-24); }); //groupBy filename without timestamp
    let toDelete = Object.values(_.mapValues(grouped, (fs) => { return fs.slice(8); })).flat();
    let unlinkPromises = toDelete.map((f) => fs.unlink(path.join(dir,f)));
    await Promise.all(unlinkPromises);
  }
}


module.exports =
  { newSwapFolder : newSwapFolder
  , openFile : openFile
  , saveSwapFolder : saveSwapFolder
  , saveSwapFolderAs : saveSwapFolderAs
  , saveLegacyFolderAs : saveLegacyFolderAs
  , deleteSwapFolder : deleteSwapFolder
  , importJSON: importJSON
  , reformJSON: reformJSON
  , destroyDb: destroyDb
  , getHash: getHashWithoutStartTime
  , truncateBackups : truncateBackups
  };




/* ============================================================================
 * PRIVATE FUNCTIONS
 * ============================================================================
 */

/*
 * Date format to append to backup files
 *
 */

const dateFormatString = "_YYYY-MM-DD_HH-mm-ss";

/**
 * Enum for file types.
 * LEGACY_GKO is anything produced with version < 2.1.0
 * GKO is produced starting at 2.2.0
 */
/** @type {String} LEGACY_GKO */
const LEGACY_GKO = "LEGACY_GKO";

/** @type {String} GKO */
const GKO = "GKO";

/** @type {String} JSON_FILE */
const JSON_FILE = "JSON_FILE";

/**
 * verifyFiletype : String -> Promise ( GKO | LEGACY_GKO )
 *
 * Verify that filepath points to a valid .gko file.
 *
 * @param {String} filepath
 *
 * @returns {Promise<String>} gko filetype
 */
async function determineFiletype(filepath) {
  const filetype = await FileType.fromFile(filepath);

  if (!filetype) {
    try {
      const dbLine = await firstline(filepath);
      const dumpInfo = JSON.parse(dbLine);
      if (dumpInfo.hasOwnProperty("db_info")) {
        return {format: LEGACY_GKO, data: dumpInfo};
      }
    } catch (err) {
      if (err.message == "Unexpected end of JSON input") {
        const filecontents = await fs.readFile(filepath, "utf8");
        const jsonfile = JSON.parse(filecontents);
        return {format: JSON_FILE, data: jsonfile};
      } else {
        return {format: false}
      }
    }
  } else if (filetype.ext == "7z") {
    return {format: GKO, data: null};
  }
}

/**
 * fullpathFilename : String -> String -> String
 *
 * Given a filepath and extension (e.g. ".gko")
 * Generate a filename from a full path, replacing path separators with %.
 *
 * @param {String} filepath
 * @param {String} extension
 *
 * @returns {String} new filename for the specified filepath and extension
 */
function fullpathFilename(filepath, extension) {
  return filepath
    .split(path.sep)
    .join("%")
    .replace(extension, "")
    .replace(":", "%");
}

/**
 * getBackupPath : String -> Date -> String
 *
 * Given a filepath and timestamp
 * Return the full path of the backup.
 *
 * @param {String} filepath
 * @param {Date} timestamp
 *
 * @returns {String} path of the backup file
 */
function getBackupPath(filepath, timestamp) {
  if (filepath) {
    const { ext } = path.parse(filepath);
    const backupName =
      fullpathFilename(filepath, ext) +
      moment(timestamp).format(dateFormatString) +
      ext;

    return path.join(app.getPath("userData"), "backups", backupName);
  } else {
    // Never-saved/Untitled
    const backupName =
      "Untitled" + moment(timestamp).format(dateFormatString) + ".gko";
    return path.join(app.getPath("userData"), "backups", backupName);
  }
}

/**
 * makeBackup : String -> Promise String Error
 *
 * Create a copy in userData, with the following contained as the filename:
 *   - the fullpathFilename
 *   - the original filename
 *   - the original file's last_modified date
 *
 * @param {string} filepath
 *
 * @returns {Promise<String, Error>}
 */
async function makeBackup(filepath) {
  const originalStats = await fs.stat(filepath);
  const backupPath = getBackupPath(filepath, originalStats.mtimeMs);

  const copyResult = await fs.copy(filepath, backupPath, { overwrite: true });

  return copyResult;
}

/**
 * swapFolderCheck : String -> Promise String Error
 *
 * Given a swapFolderPath
 * Checks that there's no existing swap folder there.
 * Return swap folder path if successful.
 * Throw an error if the swap folder already exists.
 *
 * @param {String} swapFolderPath
 *
 * @returns {Promise<String, Error>} swapFolderPath if successful
 *
 */
async function swapFolderCheck(swapFolderPath) {
  const exists = await fs.pathExists(swapFolderPath);

  if (exists) {
    throw new GingkoError(
      "Swap folder already exists.\nEither document is already open, or it failed to close properly in the past.",
      swapFolderPath
    );
  } else {
    return swapFolderPath;
  }
}

/**
 * swapCopy : String -> String -> Bool -> Promise String Error
 *
 * Given originalSwapFolderPath and newFilepath
 * - Get newSwapFolderPath from newFilepath
 * - Check the newSwapFolderPath
 * - Copy swapFolderPath to newSwapFolderPath
 * - Set swap.json filepath attribute
 *
 * @param {String} originalSwapFolderPath
 * @param {String} newFilepath
 * @param {boolean} isLegacy
 *
 * @return {Promise<String, Error>} newSwapFolderPath if successful
 *
 */
async function swapCopy(originalSwapFolderPath, newFilepath, isLegacy) {
  const newSwapName = fullpathFilename(newFilepath);
  const newSwapFolderPath = path.join(app.getPath("userData"), newSwapName);

  try {
    await swapFolderCheck(newSwapFolderPath);
    const newPath = path.join(newSwapFolderPath, isLegacy ? "leveldb" : "");

    await fs.copy(originalSwapFolderPath, newPath);
    addFilepathToSwap(newFilepath, newSwapFolderPath);

    return newSwapFolderPath;
  } catch (err) {
    throw new Error(
      "Could not create new swap folder.\n" +
        originalSwapFolderPath +
        "\n" +
        err.message
    );
  }
}

/**
 * extractFile : String -> String -> Promise String Error
 *
 * Given a filepath and targetPath
 * Extract that filepath to targetPath.
 * Return a the targetPath if successful.
 * Throw an error otherwise.
 *
 * @param {String} filepath
 * @param {String} targetPath
 *
 * @return {Promise<String, Error>} targetPath if successful
 */
async function extractFile(filepath, targetPath) {
  await execFile(path7za, ["x", "-bd", `-o${targetPath}`, filepath]);

  return targetPath;
}

/**
 * zipFolder : String -> String -> Promise String Error
 *
 * Create 7z *.gko file.
 *
 * @param {String} swapFolderPath
 * @param {String} targetPath
 *
 * @returns {Promise<String, Error>} targetPath if successful
 */
async function zipFolder(swapFolderPath, targetPath) {
  const args = ["a", targetPath, swapFolderPath + path.sep + "*", "-r"]; // TODO: exclude swap.json
  await execFile(path7za, args);

  return targetPath;
}

/**
 * addFilepathToSwap : String -> String -> ( String | Error )
 *
 * Given filepath and swapFolderPath
 * Add swap.json in swapFolderPath, with filepath attribute.
 *
 * @param {String} filepath
 * @param {String} swapFolderPath
 *
 * @returns {String | Error} filepath if successful
 */
function addFilepathToSwap(filepath, swapFolderPath) {
  try {
    const swapStore = new Store({ name: "swap", cwd: swapFolderPath });
    swapStore.set("filepath", filepath);
    return filepath;
  } catch (err) {
    throw new Error(
      "Could not set original filepath in swap folder.\n" +
        path.join(swapFolderPath, "swap.json")
    );
  }
}

/**
 * getFilepathFromSwap : String -> ( String | Null )
 *
 * @param {String} swapFolderPath
 *
 * @returns {String | null} filepath if it exists in swapFolderPath/swap.json
 *
 */
function getFilepathFromSwap(swapFolderPath) {
  const swapStore = new Store({ name: "swap", cwd: swapFolderPath });
  const filepath = swapStore.get("filepath");

  if (filepath) {
    return filepath;
  } else {
    return null;
  }
}

async function importJSON(filepath) {
  const data = await fs.readFile(filepath);

  const hash = crypto.createHash("sha1");
  hash.update(data + Date.now());
  const dbName = hash.digest("hex");
  const docName = path.basename(filepath, ".json");
  const swapFolderPath = path.join(app.getPath("userData"), dbName);
  const dbPath = path.join(swapFolderPath, "leveldb");

  await fs.ensureDir(dbPath);
  new Store({ name: "meta", cwd: swapFolderPath, defaults: { version: 1 } });
  const db = new PouchDB(dbPath);
  db.close();

  let newRoot = reformJSON(data);

  return {
    swapFolderPath: swapFolderPath,
    docName: docName,
    jsonImportData: newRoot
  };
}

function reformJSON(jsonData) {
  let nextId = 1;

  const seed = JSON.parse(
    JSON.stringify(jsonData).replace(/{(\s*)"content":/g, s => {
      return `{"id":"${nextId++}","content":`;
    })
  );

  return { id: "0", content: "", children: seed };
}
