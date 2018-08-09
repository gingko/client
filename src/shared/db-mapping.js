const fs = require("fs-extra");
const path = require("path");
const Store = require('electron-store')
const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync


const dbMap = new Store({name: "document-list"});

dbMap.getNoDot = function (key, val, def) {
  key = key.replace(".", "\\.");
  return this.get(key, val, def);
};

dbMap.setNoDot = function (key, val) {
  key = key.replace(".", "\\.");
  return this.set(key, val);
};

dbMap.hasNoDot = function (key) {
  key = key.replace(".", "\\.");
  return this.has(key);
};

dbMap.deleteNoDot = function (key) {
  key = key.replace(".", "\\.");
  return this.delete(key);
};


/*
 * addFileToDocList : String -> String
 *
 * Given a filepath
 * Add it to the list of "Recent Documents".
 */

async function addFileToDocList (filepath) {
  if(dbMap.hasNoDot(filepath)) {
    return;
  }

  const fileStats = await fs.stat(filepath);

  const newDocument =
    { "name" : path.basename(filepath)
    , "state" : "active"
    , "created_at" : (new Date(fileStats.birthtime)).toJSON()
    , "last_modified" : (new Date(fileStats.mtime)).toJSON()
    };

  dbMap.setNoDot(filepath, newDocument);

  return filepath;
}




/**
 * newDb adds a new key to the dbMap, and returns that key if successful
 * @param {string} [dbname] - database name to use as key in document-list
 * @returns {string} - database name, if successful
 */
function newDb( dbName , docName ) {
  dbName = dbName || sha1(Date.now()+machineIdSync())
  docName = docName || null

  if(dbMap.hasNoDot(dbName)) {
    throw new Error(`Cannot create db named : ${dbName}. Key already exists.`)
    return;
  }

  let nowDate = (new Date()).toJSON()
  let newDocument = { name: docName, state: "active", created_at: nowDate, last_modified: nowDate }
  dbMap.setNoDot(dbName, newDocument)

  if(dbMap.hasNoDot(dbName)) {
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
  if(!dbMap.hasNoDot(dbname)) {
    throw new Error(`Cannot rename document. Key ${dbname} not found.`)
  }

  let currentDoc = dbMap.getNoDot(dbname)

  if(currentDoc.name !== newDocName) {
    currentDoc.name = newDocName
    dbMap.setNoDot(dbname, currentDoc)
  }
}


/**
 * setModified sets the last_modified field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} [modifiedDate] - date to use in last_modified field
 */
function setModified( dbname, modifiedDate ) {
  if(dbMap.hasNoDot(dbname)) {
    modifiedDate = modifiedDate || (new Date())
    let modifiedDateString = modifiedDate.toJSON()

    let currentDoc = dbMap.getNoDot(dbname)

    if(currentDoc.last_modified !== modifiedDateString) {
      currentDoc.last_modified = modifiedDateString
      dbMap.setNoDot(dbname, currentDoc)
    }
  }
}


/**
 * setState sets the state field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} newState
 */
function setState( dbname, newState ) {
  if(!dbMap.hasNoDot(dbname)) {
    throw new Error(`Cannot set document state. Key ${dbname} not found.`)
    return;
  }

  let currentDoc = dbMap.getNoDot(dbname)

  if(currentDoc.state !== newState) {
    currentDoc.state = newState;
    dbMap.setNoDot(dbname, currentDoc);
  }
}


function removeDb( dbname ) {
  if(!dbMap.hasNoDot(dbname)) {
    throw new Error(`Document data already deleted. Key ${dbname} not found.`);
  }

  dbMap.deleteNoDot(dbname);
}


module.exports =
  { addFileToDocList : addFileToDocList
  , newDb : newDb
  , getDocList : getDocList
  , renameDoc : renameDoc
  , setModified : setModified
  , setState : setState
  , removeDb : removeDb
  }
