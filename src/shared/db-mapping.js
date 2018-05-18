const Store = require('electron-store')
const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync


const dbMap = new Store({name: "document-list"})


/**
 * newDb adds a new key to the dbMap, and returns that key if successful
 * @param {string} [dbname] - database name to use as key in document-list
 * @returns {string} - database name, if successful
 */
function newDb( dbName , docName ) {
  dbName = dbName || sha1(Date.now()+machineIdSync())
  docName = docName || null

  if(dbMap.has(dbName)) {
    throw new Error(`Cannot create db named : ${dbName}. Key already exists.`)
    return;
  }

  let nowDate = (new Date()).toJSON()
  let newDocument = { name: docName, state: "active", created_at: nowDate, last_modified: nowDate }
  dbMap.set(dbName, newDocument)

  if(dbMap.has(dbName)) {
    return dbName;
  } else {
    throw new Error(`Could not add db to document-list.json`)
    return;
  }
}


/**
 * getDocList gets an array that represents the database-to-document mapping
 */
function getDocList() {
  let docObject = dbMap.store
  var keys = Object.keys(docObject)
  var docList = keys.map((k) => { return [k, docObject[k]] })
  return docList
}


/**
 * renameDoc changes the document name for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {string} newDocName
 */
function renameDoc( dbname, newDocName ) {
  if(!dbMap.has(dbname)) {
    throw new Error(`Cannot rename document. Key ${dbname} not found.`)
    return;
  }

  let currentDoc = dbMap.get(dbname)

  if(currentDoc.name !== newDocName) {
    currentDoc.name = newDocName
    dbMap.set(dbname, currentDoc)
  }
}


/**
 * setModified sets the last_modified field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} [modifiedDate] - date to use in last_modified field
 */
function setModified( dbname, modifiedDate ) {
  if(!dbMap.has(dbname)) {
    throw new Error(`Cannot set document modified date. Key ${dbname} not found.`)
    return;
  }

  modifiedDate = modifiedDate || (new Date())
  let modifiedDateString = modifiedDate.toJSON()

  let currentDoc = dbMap.get(dbname)

  if(currentDoc.last_modified !== modifiedDateString) {
    currentDoc.last_modified = modifiedDateString
    dbMap.set(dbname, currentDoc)
  }
}


/**
 * setState sets the state field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} newState
 */
function setState( dbname, newState ) {
  if(!dbMap.has(dbname)) {
    throw new Error(`Cannot set document state. Key ${dbname} not found.`)
    return;
  }

  let currentDoc = dbMap.get(dbname)

  if(currentDoc.state !== newState) {
    currentDoc.state = newState
    dbMap.set(dbname, currentDoc)
  }
}


function removeDb( dbname ) {
  if(!dbMap.has(dbname)) {
    throw new Error(`Document data already deleted. Key ${dbname} not found.`)
    return;
  }

  dbMap.delete(dbname)
}


module.exports =
  { newDb : newDb
  , getDocList : getDocList
  , renameDoc : renameDoc
  , setModified : setModified
  , setState : setState
  , removeDb : removeDb
  }
