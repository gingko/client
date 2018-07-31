const {app} = require('electron')
const fs = require("fs-extra");
const path = require('path')
const execFile = promisify(require("child_process").execFile);
const {promisify} = require('util')
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
 * Returns the new swapFolderPath if successful.
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
    await addFilepathToSwap(filepath, swapFolderPath);
    return swapFolderPath;
  } catch (err) {
    throw err;
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
  let parsedPath = path.parse(filepath);
  let originalStats = fs.statSync(filepath);
  let backupName = fullpathFilename(filepath, ".gko") + moment(originalStats.mtimeMs).format("_YYYY-MM-DD_HH-MM-SS") + parsedPath.ext;
  let backupPath = path.join(app.getPath("userData"), backupName);

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
    throw new Error("Swap folder already exists.\nThis is likely due to a failed exit.");
  } else {
    return swapFolderPath;
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
    throw err;
  }
}




/*
 * addFilepathToSwap : String -> String -> Promise String Error
 *
 * Given filepath and swapFolderPath
 * Add swap.json in swapFolderPath, with filepath attribute.
 *
 */

async function addFilepathToSwap (filepath, swapFolderPath) {
  try {
    new Store({name: "swap", cwd: swapFolderPath, defaults: { "filepath" : filepath }});
    return filepath;
  } catch (err) {
    throw err;
  }
}




/*
 * getFilepathFromSwap : String -> Promise String Error
 *
 * Given swapFolderPath
 * Return filepath from swapFolderPath/swap.json.
 *
 */

async function getFilepathFromSwap (swapFolderPath) {
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


