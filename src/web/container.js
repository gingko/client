const _ = require("lodash");
import PouchDB from "pouchdb";

var userStoreLocal;
var userStoreRemote;
const userSettingsId = "settings";
const userStore = {
  db: function(localDb, remoteDb) {
    userStoreLocal = localDb;
    userStoreRemote = remoteDb;
  },
  load: async function() {
    return userStoreLocal.get(userSettingsId);
  },
  set: function(key, val) {
    userStoreLocal.get(userSettingsId)
      .then(doc => {
        doc[key] = val;
        userStoreLocal.put(doc);
      })
      .catch(err => {
        if (err.status == 404) {
          let newSettings = {_id : userSettingsId};
          newSettings[key] = val;
          userStoreLocal.put(newSettings);
        }
      })
  }
};

var localDB;
var localStoreId;
var treeId;
const localStore = {
  db: (db, tree_id) => {
    localDB = db;
    treeId = tree_id;
    localStoreId = `_local/${tree_id}/settings`;
  },
  load: async () => {
    let store = await localDB.get(localStoreId).catch(() => {
      return { _id: localStoreId };
    });
    return _.omit(store, ["_id", "_rev"]);
  },
  get: async (key, fallback) => {
    let store = await localDB.get(localStoreId).catch(async (e) => e);
    if (!store.error && typeof store[key] !== "undefined") {
      return store[key];
    } else {
      return fallback;
    }
  },
  set: async (key, value) => {
    let store = await localDB.get(localStoreId).catch(() => {
      return { _id: localStoreId };
    });
    store[key] = value;
    let putRes = await localDB.put(store).catch(async (e) => e);
    return putRes;
  },
};

const getInitialDocState = () => {
  const url = new URL(window.location);
  const treeName = url.searchParams.get("treeId") || "defaultTree";
  var docState = {
    dbPath: [treeName],
    lastSavedToFile: 0,
    changed: false,
    jsonImportData: false,
  };
  return docState;
};

const showMessageBox = (...args) => {
  if (args[0] && args[0].buttons && args[0].buttons.length == 1) {
    alert(`${args[0].title}\n${args[0].message}\n${args[0].detail}`);
  } else {
    console.log("showMessageBox", args);
  }
};

const justLog = (...args) => {
  //console.debug("container", ...args);
};

export {
  justLog as sendTo,
  justLog as msgWas,
  justLog as answerMain,
  getInitialDocState,
  localStore,
  userStore,
  justLog as openExternal,
  showMessageBox,
  justLog as exportDocx,
  justLog as exportJson,
  justLog as exportTxt,
};
