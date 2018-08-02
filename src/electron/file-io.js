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
const GingkoError  = require("../shared/errors");

const PouchDB = require('pouchdb');
const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)




/* ============================================================================
 * EXPOSED FUNCTIONS
 * ============================================================================
 */




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
  if (!verifyFiletype(filepath)) {
    throw new Error("Not a valid .gko file\nPossibly using legacy format.");
  }

  const swapName = fullpathFilename(filepath);
  const swapFolderPath = path.join(app.getPath("userData"), swapName );

  try {
    await makeBackup(filepath);
    await swapFolderCheck(swapFolderPath);
    await extractFile(filepath, swapFolderPath);
    addFilepathToSwap(filepath, swapFolderPath);
    return swapFolderPath;
  } catch (err) {
    throw err;
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
    const newSwapFolderPath = await swapMove(originalSwapFolderPath, newTargetPath);
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
 * deleteSwapFolder : String -> Promise String Error
 *
 * Given swapFolderPath
 * Delete it.
 *
 */

async function deleteSwapFolder (swapFolderPath) {
  try {
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
  { openFile : openFile
  , saveSwapFolder : saveSwapFolder
  , saveSwapFolderAs : saveSwapFolderAs
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
 * verifyFiletype : String -> Bool
 *
 * Verify that filepath points to a valid .gko file.
 * TODO: Handle legacy .gko files, and return correct handlers.
 *
 */

function verifyFiletype (filepath) {
  const filetype = fileType(readChunk.sync(filepath, 0, 4100));
  if(filetype.ext == "7z") {
    return true;
  } else {
    return false;
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
 * swapMove : String -> String -> Promise String Error
 *
 * Given originalSwapFolderPath and newFilepath
 * - Get newSwapFolderPath from newFilepath
 * - Check the newSwapFolderPath
 * - Copy swapFolderPath to newSwapFolderPath
 * - Delete originalSwapFolderPath
 * - Set swap.json filepath attribute
 *
 * Returns newSwapFolderPath if successful
 *
 */

async function swapMove (originalSwapFolderPath, newFilepath) {
  const newSwapName = fullpathFilename(newFilepath);
  const newSwapFolderPath = path.join(app.getPath("userData"), newSwapName );

  try {
    await swapFolderCheck(newSwapFolderPath);
    await fs.copy(originalSwapFolderPath, newSwapFolderPath);
    await deleteSwapFolder(originalSwapFolderPath);
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
  var dbLine = await firstline(filepath)
  var dumpInfo= JSON.parse(dbLine)
  const hash = crypto.createHash('sha1')
  hash.update(dumpInfo.db_info.db_name + Date.now())

  var dbName = hash.digest('hex')
  var docName = path.basename(filepath, '.gko')
  var dbPath = path.join(app.getPath('userData'), dbName)
  var db = new PouchDB(dbPath)

  var rs = fs.createReadStream(filepath)
  await db.load(rs)
  await db.close()
  return { dbName : dbName, docName : docName }
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


