const {app} = require('electron')
const fs = require('fs')
const path = require('path')
const {promisify} = require('util')
const readFile = promisify(fs.readFile)
const firstline = require('firstline')
let copyFile = promisify(fs.copyFile)
let deleteFile = promisify(fs.unlink)
const crypto = require('crypto')

const PouchDB = require('pouchdb');
const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)


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
  var dbLine = await firstline(filepath)
  var dumpInfo= JSON.parse(dbLine)
  const hash = crypto.createHash('sha1')
  hash.update(dumpInfo.db_info.db_name + "\n" + dumpInfo.start_time)

  var dbName = hash.digest('hex')
  var docName = path.basename(filepath, '.gko')
  var dbPath = path.join(app.getPath('userData'), dbName)
  var db = new PouchDB(dbPath)

  var rs = fs.createReadStream(filepath)
  await db.load(rs)
  await db.close()
  return { dbName : dbName, docName : docName }
}


function destroyDb( dbName ) {
  var dbPath = path.join(app.getPath('userData'), dbName)
  return (new PouchDB(dbPath)).destroy()
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
  { dbToFile: dbToFile
  , dbFromFile: dbFromFile
  , destroyDb: destroyDb
  , getHash: getHashWithoutStartTime
  , save: save
  }
