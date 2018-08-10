const {app} = require('electron')
const fs = require("fs-extra");
const path = require('path')
const {promisify} = require('util')
const execFile = promisify(require("child_process").execFile);
const { path7za } = require("7zip-bin");
const readFile = promisify(fs.readFile)
const firstline = require('firstline')
let copyFile = promisify(fs.copyFile)
let deleteFile = promisify(fs.unlink)
const crypto = require('crypto')
const moment = require("moment");
const Store = require("electron-store");
const readChunk = require("read-chunk");
const fileType = require("file-type");
const globby = require("globby");
const GingkoError  = require("../shared/errors");

const PouchDB = require('pouchdb');
const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)




/* ============================================================================
 * EXPOSED FUNCTIONS
 * ============================================================================
 */


/*
 * newSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 * - Open a new swap folder (if it doesn't exist)
 * Return the new swapFolderPath if successful.
 *
 */

async function newSwapFolder (swapFolderPath) {
  try {
    await swapFolderCheck(swapFolderPath);
    await fs.ensureDir(path.join(swapFolderPath, "leveldb"));
    new Store({name: "meta", cwd: swapFolderPath, defaults: { "version" : 1}});
    return swapFolderPath;
  } catch (err) {
    throw err;
  }
}




/*
 * openFile : String -> Promise String Error
 *
 * Given filepath
 * - Verify the file type
 * - Open a new swap folder (if it doesn't exist)
 * - Make a backup of the original file
 * - Add filepath to swap.json
 *
 * Return the new swapFolderPath if successful.
 *
 */

async function openFile(filepath) {
  let fileFormat = await determineFiletype(filepath);
  let swapName, swapFolderPath;

  switch (fileFormat) {
    case false:
      throw new Error("Not a valid .gko file\nPossibly using legacy format.");

    case GKO:
      swapName = fullpathFilename(filepath);
      swapFolderPath = path.join(app.getPath("userData"), swapName );

      try {
        await makeBackup(filepath);
        await swapFolderCheck(swapFolderPath);
        await extractFile(filepath, swapFolderPath);
        addFilepathToSwap(filepath, swapFolderPath);
        return swapFolderPath;
      } catch (err) {
        throw err;
      }

    case LEGACY_GKO:
      swapFolderPath = await importGko(filepath);
      return swapFolderPath;
  }

}




/*
 * saveSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 * - Get targetPath from swap.json
 * - Zip folder to backupPath
 * - Copy backupPath to targetPath, with overwrite
 *
 * Return targetPath if successful
 *
 */

async function saveSwapFolder (swapFolderPath) {
  try {
    const targetPath = getFilepathFromSwap(swapFolderPath);
    const backupPath = getBackupPath(targetPath, Date.now());
    await zipFolder(swapFolderPath, backupPath);
    await fs.copy(backupPath, targetPath, { "overwrite": true });
    return targetPath;
  } catch (err) {
    throw err;
  }
}




/*
 * saveSwapFolderAs : String -> String -> Promise String Error
 *
 * Given swapFolderPath and newFilepath
 * - Move swap folder to new swap folder
 * - Delete original swap folder
 * - Zip folder to backupPath
 * - Copy backupPath to newFilepath, with overwrite
 *
 * Returns newSwapFolderPath if successful
 *
 */

async function saveSwapFolderAs (originalSwapFolderPath, newTargetPath) {
  try {
    const newSwapFolderPath = await swapCopy(originalSwapFolderPath, newTargetPath);
    const backupPath = getBackupPath(newTargetPath, Date.now());
    await deleteSwapFolder(originalSwapFolderPath);
    await zipFolder(newSwapFolderPath, backupPath);
    await fs.copy(backupPath, newTargetPath, { "overwrite": true });
    return newSwapFolderPath;
  } catch (err) {
    throw err;
  }
}



/*
 * saveLegacyFolderAs : String -> String -> String -> Promise String Error
 *
 * Given legacyFolderPath, legacyName, and newTargetPath
 * - Copy swap folder to new swap folder
 * - Nest new swap folder into 'leveldb' path
 * - Add meta.json with file format version
 * - Zip folder to backupPath
 * - Copy backupPath to newFilepath, with overwrite
 */

async function saveLegacyFolderAs (legacyFolderPath, legacyName, newTargetPath) {
  try {
    const newSwapFolderPath = await swapCopy(legacyFolderPath, newTargetPath, true);
    new Store({name: "meta", cwd: newSwapFolderPath, defaults: { "version" : 1}});
    const backupPath = getBackupPath(newTargetPath, Date.now());
    await zipFolder(newSwapFolderPath, backupPath);
    await fs.copy(backupPath, newTargetPath);
    return newSwapFolderPath;
  } catch (err) {
    throw err;
  }
}




/*
 * deleteSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 *  - Verify that it is inside userData folder
 *  - Verify that it contains "leveldb" subdirectory
 *  - Verify that it contains "swap.json" file
 *  - Verify that it contains "meta.json" file
 * If so, delete it.
 *
 */

async function deleteSwapFolder (swapFolderPath) {
  try {
    const relative = path.relative(app.getPath("userData"), swapFolderPath);

    // Determine if a path is subdirectory of another in Node.js
    // https://stackoverflow.com/a/45242825/43769
    if (!(!!relative && !relative.startsWith("..") && !path.isAbsolute(relative))) {
      return;
    }

    if (!fs.pathExistsSync(path.join(swapFolderPath, "leveldb"))) {
      return;
    }

    if (!fs.pathExistsSync(path.join(swapFolderPath, "swap.json"))) {
      return;
    }

    if (!fs.pathExistsSync(path.join(swapFolderPath, "meta.json"))) {
      return;
    }


    await fs.remove(swapFolderPath);
    return swapFolderPath;
  } catch (err) {
    throw new Error("Could not delete swap folder.\n" + swapFolderPath);
  }
}




function dbToFile(database, filepath) {
  return new Promise((resolve, reject) => {
    let ws = fs.createWriteStream(filepath)
    ws.on('error', reject)

    database.dump(ws)
      .then(() => { resolve(filepath) })
      .catch(reject)
  })
}




async function dbFromFile(filepath) {
  try {
    var importResult = await importGko(filepath);
  } catch (err) {
    if(err.message == "Unexpected end of JSON input") {
      importResult = await importJSON(filepath);
    }
  }
  return importResult;
}




async function destroyDb( dbName ) {
  var dbPath = path.join(app.getPath('userData'), dbName)
  try {
    await deleteFile(path.join(app.getPath('userData'), `window-state-${dbName}.json`));
  } finally {
    return (new PouchDB(dbPath)).destroy()
  }
}




function getHashWithoutStartTime(filepath) {
  return new Promise(async (resolve, reject) => {
    try {
      const hash = crypto.createHash('sha1')
      let filecontents = await readFile(filepath, 'utf8')
      let transformedContents = filecontents.replace(/"start_time":".*","db_info"/, '"start_time":"","db_info"')
      hash.update(transformedContents)
      resolve(hash.digest('base64'))
    } catch (err) {
      reject(err)
    }
  })
}




function save(database, filepath) {
  return new Promise(async (resolve, reject) => {
    try {
      let datestring = new Date().toJSON()
      let temppath1 = filepath + datestring + ".swp1"
      let temppath2 = filepath + datestring + ".swp2"

      await dbToFile(database, temppath1)
      await dbToFile(database, temppath2)

      let hash1 = await getHashWithoutStartTime(temppath1)
      let hash2 = await getHashWithoutStartTime(temppath2)

      if (hash1 == hash2) {
        await copyFile(temppath1, filepath)
        let del1 = deleteFile(temppath1)
        let del2 = deleteFile(temppath2)
        await Promise.all([del1, del2])
        var finalHash = await getHashWithoutStartTime(filepath)

        if(hash1 == finalHash) {
          resolve({path: filepath, hash: finalHash})
        } else {
          reject(Error(`Integrity check failed on save: ${hash1} !== ${finalHash}`))
        }
      } else {
        reject(Error(`Integrity check failed on dbToFile: ${hash1} !== ${hash2}`))
      }
    } catch(err) {
      reject(err)
    }
  })
}





module.exports =
  { newSwapFolder : newSwapFolder
  , openFile : openFile
  , saveSwapFolder : saveSwapFolder
  , saveSwapFolderAs : saveSwapFolderAs
  , saveLegacyFolderAs : saveLegacyFolderAs
  , deleteSwapFolder : deleteSwapFolder
  , dbToFile: dbToFile
  , dbFromFile: dbFromFile
  , destroyDb: destroyDb
  , getHash: getHashWithoutStartTime
  , save: save
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



/*
 * Enum for file types.
 * LEGACY_GKO is anything produced with version < 2.1.0
 * GKO is produced starting at 2.2.0
 *
 */

const LEGACY_GKO = "LEGACY_GKO";
const GKO = "GKO";





/*
 * verifyFiletype : String -> ( GKO | LEGACY_GKO )
 *
 * Verify that filepath points to a validelectron-store/issues .gko file.
 *
 */

async function determineFiletype (filepath) {
  const filetype = fileType(readChunk.sync(filepath, 0, 4100));

  if(!filetype) {
    var dbLine = await firstline(filepath);
    var dumpInfo= JSON.parse(dbLine);
    if(dumpInfo.hasOwnProperty("db_info")) {
      return LEGACY_GKO;
    }
  } else if(filetype.ext == "7z") {
    return GKO;
  }
}




/*
 * fullpathFilename : String -> String -> String
 *
 * Given a filepath and extension (e.g. ".gko")
 * Generate a filename from a full path, replacing path separators with %.
 *
 */

function fullpathFilename (filepath, extension) {
  return filepath.split(path.sep).join("%").replace(extension,"");
}




/*
 * getBackupPath : String -> Date -> String
 *
 * Given a filepath and timestamp
 * Return the full path of the backup.
 *
 */

function getBackupPath (filepath, timestamp) {
  const { ext } = path.parse(filepath);
  const backupName = fullpathFilename(filepath, ext) + moment(timestamp).format(dateFormatString) + ext;
  return path.join(app.getPath("userData"), backupName);
}




/*
 * makeBackup : String -> Promise String Error
 *
 * Given a filepath
 * Create a copy in userData, with the following contained as the filename:
 *   - the fullpathFilename
 *   - the original filename
 *   - the original file's last_modified date
 *
 */

async function makeBackup (filepath) {
  let originalStats = fs.statSync(filepath);
  let backupPath = getBackupPath(filepath, originalStats.mtimeMs);

  try {
    let copyResult = await fs.copy(filepath, backupPath, { "overwrite": true });
    return copyResult;
  } catch (err) {
    throw err;
  }
}




/*
 * swapFolderCheck : String -> Promise String Error
 *
 * Given a swapFolderPath
 * Checks that there's no existing swap folder there.
 * Return swap folder path if successful.
 * Throw an error if the swap folder already exists.
 *
 */

async function swapFolderCheck (swapFolderPath) {
  const exists = await fs.pathExists(swapFolderPath);

  if (exists) {
    throw new GingkoError("Swap folder already exists.\nEither document is already open, or it failed to close properly in the past.", swapFolderPath);
  } else {
    return swapFolderPath;
  }
}




/*
 * swapCopy : String -> String -> Promise String Error
 *
 * Given originalSwapFolderPath and newFilepath
 * - Get newSwapFolderPath from newFilepath
 * - Check the newSwapFolderPath
 * - Copy swapFolderPath to newSwapFolderPath
 * - Set swap.json filepath attribute
 *
 * Returns newSwapFolderPath if successful
 *
 */

async function swapCopy (originalSwapFolderPath, newFilepath, isLegacy) {
  const newSwapName = fullpathFilename(newFilepath);
  const newSwapFolderPath = path.join(app.getPath("userData"), newSwapName );

  try {
    await swapFolderCheck(newSwapFolderPath);
    await fs.copy(originalSwapFolderPath, path.join(newSwapFolderPath, isLegacy ? "leveldb" : ""));
    addFilepathToSwap(newFilepath, newSwapFolderPath);
    return newSwapFolderPath;
  } catch (err) {
    throw new Error("Could not create new swap folder.\n" + originalSwapFolderPath);
  }
}




/*
 * extractFile : String -> String -> Promise String Error
 *
 * Given a filepath and targetPath
 * Extract that filepath to targetPath.
 * Return a the targetPath if successful.
 * Throw an error otherwise.
 *
 */

async function extractFile (filepath, targetPath) {
  try {
    await execFile(path7za, ["x","-bd", `-o${targetPath}`, filepath ]);
    return targetPath;
  } catch (err) {
    throw err;
  }
}




/*
 * zipFolder : String -> String -> Promise String Error
 *
 * Given swapFolderPath and targetPath
 * Create 7z *.gko file.
 * Return targetPath if successful.
 *
 */

async function zipFolder (swapFolderPath, targetPath) {
  let args =
      [ "a"
      , targetPath
      , swapFolderPath + path.sep + "*"
      , "-r"
      ]; // TODO: exclude swap.json

  try {
    await execFile(path7za, args);
    return targetPath;
  } catch (err) {
    throw err;
  }
}




/*
 * addFilepathToSwap : String -> String -> ( String | Error )
 *
 * Given filepath and swapFolderPath
 * Add swap.json in swapFolderPath, with filepath attribute.
 *
 */

function addFilepathToSwap (filepath, swapFolderPath) {
  try {
    const swapStore = new Store({name: "swap", cwd: swapFolderPath});
    swapStore.set("filepath", filepath);
    return filepath;
  } catch (err) {
    throw new Error("Could not set original filepath in swap folder.\n" + path.join(swapFolderPath, "swap.json"));
  }
}




/*
 * getFilepathFromSwap : String -> ( String | Error )
 *
 * Given swapFolderPath
 * Return filepath from swapFolderPath/swap.json.
 *
 */

function getFilepathFromSwap (swapFolderPath) {
  const swapStore = new Store({name: "swap", cwd: swapFolderPath});
  let filepath = swapStore.get("filepath");
  if (filepath) {
    return filepath;
  } else {
    throw new Error("Could not get original filepath from swap folder.\n" + path.join(swapFolderPath, "swap.json"));
  }
}




async function importGko(filepath) {
  await makeBackup(filepath);
  var dbLine = await firstline(filepath);
  var dumpInfo= JSON.parse(dbLine);
  const hash = crypto.createHash("sha1");
  hash.update(dumpInfo.db_info.db_name + Date.now());

  var dbName = hash.digest("hex");
  var swapFolderPath = path.join(app.getPath("userData"), dbName);
  var dbPath = path.join(swapFolderPath, "leveldb");

  await fs.ensureDir(dbPath);
  new Store({name: "meta", cwd: swapFolderPath, defaults: { "version" : 1}});
  addFilepathToSwap(filepath, swapFolderPath);
  var db = new PouchDB(dbPath);

  var rs = fs.createReadStream(filepath);
  await db.load(rs);
  await db.close();
  return swapFolderPath;
}


async function importJSON(filepath) {
  let data = await readFile(filepath);

  const hash = crypto.createHash('sha1')
  hash.update(data + Date.now())
  var dbName = hash.digest('hex')
  var docName = path.basename(filepath, '.json')

  let nextId = 1

  let seed =
    JSON.parse(
        data.toString()
            .replace( /{(\s*)"content":/g
                    , s => {
                        return `{"id":"${nextId++}","content":`
                      }
                    )
      )

  let newRoot =
        { id: "0"
        , content: ""
        , children: seed
        }

  return { dbName : dbName, docName : docName , jsonImportData : newRoot };
}


