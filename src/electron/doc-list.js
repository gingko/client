const fs = require("fs-extra");
const path = require("path");
const _ = require("lodash");
const Store = require('electron-store')
const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync


const docList = new Store({name: "document-list"});

docList.getNoDot = function (key, val, def) {
  key = key.replace(/\./g, "\\.");
  return this.get(key, val, def);
};

docList.setNoDot = function (key, val) {
  key = key.replace(/\./g, "\\.");
  return this.set(key, val);
};

docList.hasNoDot = function (key) {
  key = key.replace(/\./g, "\\.");
  return this.has(key);
};

docList.deleteNoDot = function (key) {
  key = key.replace(/\./g, "\\.");
  return this.delete(key);
};


/*
 * addFileToDocList : String -> String
 *
 * Given a filepath
 * Add it to the list of "Recent Documents" if it doesn't exist.
 */

async function addFileToDocList (filepath) {
  if(docList.hasNoDot(filepath)) {
    return;
  }

  const fileStats = await fs.stat(filepath);

  const newDocument =
    { "name" : path.basename(filepath)
    , "state" : "active"
    , "created_at" : (new Date(fileStats.birthtime)).toJSON()
    , "last_modified" : (new Date(fileStats.mtime)).toJSON()
    , "last_opened" : (new Date()).toJSON()
    };

  docList.setNoDot(filepath, newDocument);

  return filepath;
}




/**
 * newDb adds a new key to the docList, and returns that key if successful
 * @param {string} [dbname] - database name to use as key in document-list
 * @returns {string} - database name, if successful
 */
function newDb( dbName , docName ) {
  dbName = dbName || sha1(Date.now()+machineIdSync())
  docName = docName || null

  if(docList.hasNoDot(dbName)) {
    throw new Error(`Cannot create db named : ${dbName}. Key already exists.`)
    return;
  }

  let nowDate = (new Date()).toJSON()
  let newDocument = { name: docName, state: "active", created_at: nowDate, last_modified: nowDate }
  docList.setNoDot(dbName, newDocument)

  if(docList.hasNoDot(dbName)) {
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
  let docObject = docList.store;
  var keys = Object.keys(docObject);
  var theList = keys.map((k) => {
    if (!_.has(docObject[k], "last_opened") && _.has(docObject[k], "last_modified")) {
      _.set(docObject[k], "last_opened", docObject[k].last_modified);
    }
    return [k, docObject[k]];
  });
  return theList;
}



function getRecentDocs() {
  let docObject = docList.store;

  let keys = Object.keys(docObject);
  let theList = keys.map((k) => {
    if(docObject[k].state !== "active") {
      return false;
    }

    if (!_.has(docObject[k], "last_opened") && _.has(docObject[k], "last_modified")) {
      _.set(docObject[k], "last_opened", docObject[k].last_modified);
    }
    return { location: k, name: docObject[k].name, last_opened: docObject[k].last_opened };
  });

  return _.sortBy(theList, "last_opened").reverse().filter(x => !!x);
}


/**
 * setOpened set the last_opened field of a document for a given key
 *
 */

function setOpened( dbname, openedDate ) {
  if(docList.hasNoDot(dbname)) {
    openedDate = openedDate || (new Date());
    let openedDateString = openedDate.toJSON();

    let currentDoc = docList.getNoDot(dbname);

    if(currentDoc.last_opened !== openedDateString) {
      currentDoc.last_opened = openedDateString;
      docList.setNoDot(dbname, currentDoc);
    }
  }
}


/**
 * setModified sets the last_modified field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} [modifiedDate] - date to use in last_modified field
 */
function setModified( dbname, modifiedDate ) {
  if(docList.hasNoDot(dbname)) {
    modifiedDate = modifiedDate || (new Date())
    let modifiedDateString = modifiedDate.toJSON()

    let currentDoc = docList.getNoDot(dbname)

    if(currentDoc.last_modified !== modifiedDateString) {
      currentDoc.last_modified = modifiedDateString
      docList.setNoDot(dbname, currentDoc)
    }
  }
}


/**
 * setState sets the state field of a document for a given database key
 * @param {string} dbname - database name used as key in document-list
 * @param {Date} newState
 */
function setState( dbname, newState ) {
  if(!docList.hasNoDot(dbname)) {
    throw new Error(`Cannot set document state. Key ${dbname} not found.`)
    return;
  }

  let currentDoc = docList.getNoDot(dbname)

  if(currentDoc.state !== newState) {
    currentDoc.state = newState;
    docList.setNoDot(dbname, currentDoc);
  }
}


function removeDb( dbname ) {
  if(!docList.hasNoDot(dbname)) {
    throw new Error(`Document data already deleted. Key ${dbname} not found.`);
  }

  docList.deleteNoDot(dbname);
}


module.exports =
  { addFileToDocList : addFileToDocList
  , newDb : newDb
  , getDocList : getDocList
  , getRecentDocs : getRecentDocs
  , setOpened : setOpened
  , setModified : setModified
  , setState : setState
  , removeDb : removeDb
  }
