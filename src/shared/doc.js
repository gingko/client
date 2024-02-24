import * as data from "./data.js";
//import Worker from "worker-loader!./data.worker.js";
import hlc from '@tpp/hybrid-logical-clock';
import uuid from '@tpp/simple-uuid';
const dataWorker = new Worker('./data.worker.js');

const _ = require("lodash");
const Mousetrap = require("mousetrap");
const screenfull = require("screenfull");
const container = require("Container");
const platform = require("platform");
const config = require("../../config.js");
const mycrypt = require("./encrypt.js");
const PersistentWebSocket = require("pws");

// Initialize Error Reporting
import * as Sentry from '@sentry/browser';
import LogRocket from 'logrocket';

if(window.location.origin === config.PRODUCTION_SERVER) {
  Sentry.init({ dsn: config.SENTRY_DSN
    , integrations: [new Sentry.BrowserTracing()]
    , tracesSampleRate: 1.0
  });

  LogRocket.init(config.LOGROCKET_APPID);
  LogRocket.getSessionURL(sessionURL => {
    Sentry.configureScope(scope => {
      scope.setExtra("sessionURL", sessionURL);
    });
  });
}

import PouchDB from "pouchdb";
const Dexie = require("dexie").default;
import { ImmortalStorage, IndexedDbStore, LocalStorageStore, SessionStorageStore } from 'immortal-db';
let ImmortalDB;
async function initImmortalDB() {
  const immortalStores = [await new IndexedDbStore(), await new LocalStorageStore(), await new SessionStorageStore()];
  ImmortalDB = new ImmortalStorage(immortalStores);
}
initImmortalDB();

const dexie = new Dexie("db");
dexie.version(4).stores({
  trees: "id,updatedAt",
  cards: "updatedAt, treeId, [treeId+deleted]",
  tree_snapshots: "snapshot, treeId"
});

const helpers = require("./doc-helpers");
//import { Elm } from "../elm/Main";

/* === Global Variables === */

let lastActivesScrolled = null;
let lastColumnScrolled = null;
let ticking = false;
let renaming = false;
let lang = "en";
let tourStepPositionStepNum = false;
let tourStepPositionRefElementId = "";
window.elmMessages = [];

let remoteDB;
let db;
let gingko;
let TREE_ID;
const CLIENT_ID = uuid(12);
let COLLAB_STATE;
let DATA_TYPE;
const CARD_DATA = Symbol.for("cardbased");
const GIT_LIKE_DATA = Symbol.for("couchdb");
let userDbName;
let email = null;
let ws;
let wsQueue = [];

let PULL_LOCK = false;
let DIRTY = false;
let pushErrorCount = 0;
let loadingDocs = false;
let draggingInternal = false;
let viewportWidth = document.documentElement.clientWidth;
let viewportHeight = document.documentElement.clientHeight;
let horizontalScrollInterval;
let verticalScrollInterval;
let docElement;
let sidebarWidth;
let externalDrag = false;
let savedObjectIds = new Set();
let cardDataSubscription = null;
let historyDataSubscription = null;
let wsErrorCount = 0;
const localStore = container.localStore;
const sessionStorageKey = "gingko-session-storage";
function getDataType() {
  return DATA_TYPE;
}

/* === Initializing App === */

initElmAndPorts();

async function initElmAndPorts() {
  let flags = getFlags();

  if (flags.email) {
    await setUserDbs(flags.email);
  }

  gingko = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: flags,
  });

  // All messages from Elm
  gingko.ports.infoForOutside.subscribe(function (elmdata) {
    fromElm(elmdata.tag, elmdata.data);
  });

  // Messages from dataWorker
  dataWorker.onmessage = (e) => {
    fromElm(e.data.tag, e.data.data);
  };

  initEventListeners();
}


function getFlags() {
  let sessionMaybe = getSessionData();
  let sessionData = sessionMaybe == null ? {} : sessionMaybe;
  console.log("sessionData found", JSON.stringify(sessionData));
  if (sessionData.email) {
    email = sessionData.email;
    sessionData.sidebarOpen = (sessionData.hasOwnProperty('sidebarOpen')) ?  sessionData.sidebarOpen : false;
    sidebarWidth = sessionData.sidebarOpen ? 215 : 40;
    lang = sessionData.language || "en";
  }

  // Dynamic and global session info
  let timestamp = Date.now();
  sessionData.seed = timestamp;
  sessionData.isMac = platform.os.family === 'OS X';
  sessionData.currentTime = timestamp;
  sessionData.fromLegacy = document.referrer.startsWith(config.LEGACY_URL);
  return sessionData;
}


async function setUserDbs(eml) {
  email = eml;

  // HEAD request to /session to check if we're logged in
  let sessionResponse = await fetch("/session", { method: "HEAD" });
  if (sessionResponse.status === 401) {
    Sentry.captureMessage('401: Unauthorized', { extra: { email } });
    await logout();
    return;
  }

  userDbName = `userdb-${helpers.toHex(email)}`;
  let userDbUrl = window.location.origin + "/db/" + userDbName;
  var remoteOpts = { skip_setup: true };
  remoteDB = new PouchDB(userDbUrl, remoteOpts);
  // Check remoteDB exists and accessible before continuing
  let remoteDBinfo = await remoteDB.info().catch((e) => {console.error(e)});
  if (remoteDBinfo.error === "unauthorized") {
    await logout();
    return;
  }

  db = new PouchDB(userDbName);
  initWebSocket();

  // Sync document list with server

  let firstLoad = true;

  Dexie.liveQuery(() => dexie.trees.toArray()).subscribe((trees) => {
    const docMetadatas = trees.filter(t => t.deletedAt == null).map(treeDocToMetadata);
    if (!loadingDocs && !firstLoad) {
      toElm(docMetadatas, "documentListChanged");
    }

    const unsyncedTrees = trees.filter(t => !t.synced).map(t => _.omit(t, ['synced', 'collaborators']));
    if (unsyncedTrees.length > 0) {
      wsSend('trees', unsyncedTrees, false);
    }
    firstLoad = false;
  });

  thirdPartyScriptsInit(eml)
}


function initWebSocket () {
  const wsUrl = window.location.origin.replace('http', 'ws')+'/ws'
  ws = new PersistentWebSocket(wsUrl, {pingTimeout: 30000 + 2000})

  let interval;
  ws.onopen = () => {
    // Send each item from wsQueue and clear it
    wsQueue.forEach(([msgTag, msgData]) => {
      wsSend(msgTag, msgData, false)
    })
    wsQueue = [];

    if (TREE_ID) {
      wsSend('rt:join', { tr: TREE_ID, uid: CLIENT_ID, m: COLLAB_STATE || null }, false);
    }

    interval = setInterval(() => ws.send('ping'), 30000)
    setTimeout(() => toElm(null, 'appMsgs', 'SocketConnected') , 1000)
  }

  ws.onmessage = async (e) => {
    if (e.data == 'pong') {
      return
    }

    const data = JSON.parse(e.data)
    try {
      switch (data.t) {
        case 'user':
          console.log('user', JSON.stringify(data.d))
          let currentSessionData = getSessionData()
          if (currentSessionData && currentSessionData.email === data.d.id) {
            // Merge properties
            let newSessionData = Object.assign({}, currentSessionData, _.omit(data.d, ['id', 'createdAt']))
            if (!_.isEqual(currentSessionData, newSessionData)) {
              setSessionData(newSessionData, 'user ws msg')
              setTimeout(() => gingko.ports.userSettingsChange.send(newSessionData), 0)
            }
          }
          break

        case 'cards':
          if (data.d.length > 0) {
            await dexie.cards.bulkPut(data.d.map(c => ({ ...c, synced: true })))
          }
          break

        case 'cardsConflict':
          if (data.d.length > 0) {
            await dexie.cards.bulkPut(data.d.map(c => ({ ...c, synced: true })))

            // send encrypted unsynced local cards to Sentry
            const unsyncedCards = await dexie.cards.where('treeId').equals(TREE_ID).and(c => !c.synced).toArray();
            Sentry.captureMessage('cardsConflict: cards conflict ' + TREE_ID, { extra: { unsyncedCards , error: data.e} })
          } else {
            Sentry.captureMessage('cardsConflict: no cards ' + TREE_ID, { extra: { error: data.e} })
            const numberUnsynced = await dexie.cards.where('treeId').equals(TREE_ID).and(c => !c.synced).count();
            const msg = `Error syncing ${numberUnsynced} change${numberUnsynced == 1 ? "" : "s"}. Try refreshing the page.\n\nIf this error persists, please contact support!`;
            toElm(msg, 'appMsgs', 'ErrorAlert');
          }
          break

        case 'pushOk':
          pushErrorCount = 0;
          hlc.recv(_.max(data.d))
          toElm(data, 'appMsgs', 'PushOk')
          break

        case 'pushError':
          pushErrorCount++;
          if (pushErrorCount >= 4) {
            let numberUnsynced = await dexie.cards.where('treeId').equals(TREE_ID).and(c => !c.synced).count();
            const msg = `Error syncing ${numberUnsynced} change${numberUnsynced == 1 ? "" : "s"}. Try refreshing the page.\n\nIf this error persists, please contact support!`;
            toElm(msg, 'appMsgs', 'ErrorAlert');
          }
          console.log(pushErrorCount)
          toElm(data, 'appMsgs', 'PushError')
          break

        case 'doPull':
          // Server says this tree has changes
          if (data.d === TREE_ID) {
            let cards = await dexie.cards.where('treeId').equals(TREE_ID).toArray()
            pull(TREE_ID, getChk(TREE_ID, cards))
          }
          break

        case 'trees':
          await dexie.trees.bulkPut(data.d.map(t => ({ ...t, synced: true })))
          break

        case 'treesOk':
          await dexie.trees.where('updatedAt').belowOrEqual(data.d).modify({ synced: true })
          break

        case 'historyMeta': {
          const { tr, d } = data
          const snapshotData = d.map(hmd => ({ snapshot: hmd.id, treeId: tr, data: null }))
          try {
            await dexie.tree_snapshots.bulkAdd(snapshotData)
          } catch (e) {
            const errorNames = e.failures.map(f => f.name)
            if (errorNames.every(n => n === 'ConstraintError')) {
              // Ignore
            } else {
              throw e
            }
          }
          break
        }

        case 'history': {
          const { tr, d } = data
          const snapshotData = d.map(hd => ({
            snapshot: hd.id,
            treeId: tr,
            data: hd.d.map(d => ({ ...d, synced: true }))
          }))
          await dexie.tree_snapshots.bulkPut(snapshotData)
          break
        }

        case 'userSettingOk':
          console.log('userSettingOk', data.d)
          const { d } = data
          let currSessionData = getSessionData()
          currSessionData[d[0]] = d[1]
          setSessionData(currSessionData, 'userSettingOk ws msg')
          break

        case 'rt':
          if (Array.isArray(data.d.m) && data.d.m[0] == "d") {
            toElm(data.d.uid, 'docMsgs', 'CollaboratorDisconnected')
          } else {
            toElm(data.d, 'docMsgs', 'RecvCollabState');
          }
          break;

        case 'rt:users':
          toElm(data.d, 'docMsgs', 'RecvCollabUsers');
          break;

        case 'removedFrom':
          await dexie.trees.delete(data.d);
          if (data.d === TREE_ID) {
            location.assign('/');
          }
          break;
      }
    } catch (e) {
      console.log(e)
    }
  }

  ws.onerror = (e) => {
    Sentry.captureException(e);
    if (wsErrorCount == 3 || wsErrorCount == 10 || wsErrorCount >= 20) {
      let msg = `Error with the current session.\nTry refreshing.\n\nIf it persists, export a JSON backup of recent work, and log out and back in.`
      toElm(msg, 'appMsgs', 'ErrorAlert');
    }
    wsErrorCount++;
    console.error('ws error', e);
  }

  ws.onclose = (e) => {
    // Clear list of collaborators
    toElm([], 'docMsgs', 'RecvCollabUsers');

    clearInterval(interval)
  }
}


/* === Third-Party Scripts === */

// Stripe
const stripe = Stripe(config.STRIPE_PUBLIC_KEY);

const createCheckoutSession = function(userEmail, priceId) {
  return fetch("/create-checkout-session", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      priceId: priceId,
      customer_email: userEmail
    })
  }).then(function(result) {
    return result.json();
  });
}

// LogRocket and Beamer
function thirdPartyScriptsInit (email) {
  LogRocket.identify(email)

  if (email !== 'cypress@testing.com') {
    self.fwSettings = {
      'widget_id': config.FRESHDESK_APPID
    }
    !function () {
      if ('function' != typeof window.FreshworksWidget) {
        var n = function () {n.q.push(arguments)}
        n.q = [], window.FreshworksWidget = n
      }
    }()
    let freshdeskScript = document.createElement('script')
    freshdeskScript.setAttribute('src', `https://euc-widget.freshworks.com/widgets/${config.FRESHDESK_APPID}.js`)
    freshdeskScript.setAttribute('async', '')
    freshdeskScript.setAttribute('defer', '')
    document.head.appendChild(freshdeskScript)
    FreshworksWidget('hide', 'launcher')
  }

  if (window.location.origin === config.PRODUCTION_SERVER) {
    self.beamer_config = {
      product_id: config.BEAMER_APPID,
      selector: '#notifications-icon',
      user_id: email,
      user_email: email
    }
    let beamerScript = document.createElement('script')
    beamerScript.setAttribute('src', 'https://app.getbeamer.com/js/beamer-embed.js')
    beamerScript.setAttribute('defer', 'defer')
    document.head.appendChild(beamerScript)
  }
}


function initEventListeners () {
  window.checkboxClicked = (cardId, number) => {
    toElm([cardId, number], 'docMsgs', 'CheckboxClicked')
  }

  // Prevent closing if unsaved changes exist.
  window.addEventListener('beforeunload', (event) => {
    if (DIRTY) {
      event.preventDefault()
      event.returnValue = ''
    }
  })

  // Fullscreen change event
  // This is so that we can use "Esc" once to leave fullscreen mode.
  if (screenfull.isEnabled) {
    screenfull.on('change', () => {
      toElm(screenfull.isFullscreen, 'docMsgs', 'FullscreenChanged')
    })
  }

  window.addEventListener('beforeprint', () => {
    toElm(null, 'docMsgs', 'WillPrint')
  })
}


/* === Elm / JS Interop === */

function toElm(data, portName, tagName) {
  if (process.env.NODE_ENV === 'development') {
    console.log("toElm", portName, tagName, data);
  }

  if (!gingko) { return; }
  let portExists = gingko.ports.hasOwnProperty(portName);
  let tagGiven = typeof tagName == "string";

  if (portExists) {
    var dataToSend;

    if (tagGiven) {
      dataToSend = { tag: tagName, data: data };
    } else {
      dataToSend = data;
    }
    gingko.ports[portName].send(dataToSend);
  } else {
    console.error("Unknown port", portName, data);
  }
}

const fromElm = (msg, elmData) => {
  if (process.env.NODE_ENV === 'development') {
    console.log("fromElm", msg, elmData);
  }

  window.elmMessages.push({tag: msg, data: elmData});
  window.elmMessages = window.elmMessages.slice(-10);

  let casesWeb = {
    // === SPA ===

    StoreUser: async () => {
      setSessionData(elmData, "StoreUser");
      await setUserDbs(elmData.email);
      const timestamp = Date.now();
      elmData.seed = timestamp;
      elmData.currentTime = timestamp;
      setTimeout(() => gingko.ports.userLoggedInMsg.send(null), 0);
    },

    LogoutUser : async () => {
      await logout();
    },

    // === Dialogs, Menus, Window State ===

    Alert: () => {
      alert(elmData);
    },

    SetDirty: () => {
      DIRTY = elmData;
    },

    DragDone: () => {
      draggingInternal = false;
    },

    // === Database ===

    InitDocument: async () => {
      TREE_ID = elmData;

      const now = Date.now();
      const treeDoc = {...treeDocDefaults, id: TREE_ID, location: "cardbased", owner: email, createdAt: now, updatedAt: now};
      const cardDoc = {...cardDefaults, id: my_uuid(24), treeId: TREE_ID, updatedAt: hlc.nxt()};

      await dexie.trees.add(treeDoc);
      await dexie.cards.add(cardDoc);

      // Set localStore db
      localStore.db(elmData);

      try {
        loadCardBasedDocument(TREE_ID);
      } catch (e) {
        console.log(e);
      }
    },

    LoadDocument : async () => {
      TREE_ID = elmData;

      wsSend('rt:join', { tr: TREE_ID, uid: CLIENT_ID, m: COLLAB_STATE || null}, true);
      // Load title
      const treeDoc = await dexie.trees.get(elmData);
      if (treeDoc) {
        toElm(treeDocToMetadata(treeDoc), "appMsgs", "MetadataUpdate")
      } else {
        toElm(TREE_ID, "appMsgs", "NotFound")
        return;
      }

      try {
        if (treeDoc.location === "couchdb") {
          loadGitLikeDocument(elmData);
        } else if (treeDoc.location === "cardbased") {
          loadCardBasedDocument(elmData);
        }
      } catch (e) {
        console.log(e);
      }
    },

    CopyDocument: async () => {
      // Load title
      let metadata = await data.loadMetadata(db, elmData);

      // Load local document data.
      let localExists;
      let [loadedData, savedIds] = await data.load(db, elmData);
      savedIds.forEach(item => savedObjectIds.add(item));

      if (loadedData.hasOwnProperty("commit") && loadedData.commit.length > 0) {
        toElm([metadata.name, loadedData], "copyLoaded");
      } else {
        localExists = false;
        let remoteExists;
        PULL_LOCK = true;
        try {
          let pullResult = await data.pull(db, remoteDB, elmData, "LoadDocument");

          if (pullResult !== null) {
            pullResult[1].forEach(item => savedObjectIds.add(item));
            toElm([metadata.name, pullResult[0]], "copyLoaded");
          } else {
            remoteExists = false;
            if (!localExists && !remoteExists) {
              toElm(elmData, "appMsgs", "NotFound")
            }
          }
        } catch (e){
          console.error(e)
        } finally {
          PULL_LOCK = false;
        }
      }

    },

    GetDocumentList: () => {
      loadDocListAndSend("GetDocumentList");
    },

    RequestDelete: async () => {
      if (confirm(`Are you sure you want to delete the document '${elmData[1]}'?`)) {
        await dexie.trees.update(elmData[0], {deletedAt: Date.now(), synced: false});
      }
    },

    RenameDocument: async () => {
      if (!renaming) { // Hack to prevent double rename attempt due to Browser.Dom.blur
        renaming = true;
        await dexie.trees.update(TREE_ID, {name: elmData, updatedAt: Date.now(), synced: false});
        renaming = false;
      }
    },

    PushDeltas : () => {
      if (elmData.dlts.length > 0) {
        wsSend('push', elmData, false);
      }
    },

    SaveCardBased : async () => {
      if (DATA_TYPE === GIT_LIKE_DATA) {
        return;
      }

      if (elmData && Array.isArray(elmData.errors)) {
        alert("Error saving data!\n\n" + elmData.errors.join("\n----\n"));
        return;
      }

      if (!elmData || !elmData.toAdd || !elmData.toMarkSynced || !elmData.toMarkDeleted || !elmData.toRemove) {
        alert("Error saving data!\nInvalid data sent to DB:\n" + JSON.stringify(elmData));
        return;
      }

      let newData = elmData.toAdd.map((c) => { return { ...c, updatedAt: hlc.nxt() }})
      const toMarkSynced = elmData.toMarkSynced.map((c) => { return { ...c, synced: true }})
      const timestamp = Date.now();

      let toMarkDeleted = [];
      if (elmData.toMarkDeleted.length > 0) {
        const deleteHash = uuid();
        toMarkDeleted = elmData.toMarkDeleted.map((c, i) => ({ ...c, updatedAt: `${timestamp}:${i}:${deleteHash}` }));
      }

      try {
        await dexie.transaction('rw', dexie.cards, async () => {
            dexie.cards.bulkPut(newData.concat(toMarkSynced).concat(toMarkDeleted));
            dexie.cards.bulkDelete(elmData.toRemove);
            DIRTY = false;
        });

        if (elmData.toAdd.length > 0 || toMarkDeleted.length > 0) {
          if (elmData.toAdd.length == 1 && elmData.toAdd[0].content == "") {
            // Don't add new empty cards to history.
            return;
          }

          const cards = await dexie.cards.where({ treeId: TREE_ID, deleted: 0 }).toArray();
          const lastUpdatedTime = cards.map((c) => c.updatedAt.split(':')[0]).reduce((a, b) => Math.max(a, b));
          const snapshotId = `${lastUpdatedTime}:${TREE_ID}`;
          const snapshotData = cards.map((c) => ({ ...c, snapshot: snapshotId, delta: 0}));
          const snapshot = { snapshot: snapshotId, treeId: TREE_ID, data: snapshotData, local: true, ts: Number(lastUpdatedTime)};
          await dexie.tree_snapshots.put(snapshot);
        }
        await dexie.trees.update(TREE_ID, {updatedAt: timestamp, synced: false});
      } catch (e) {
        alert("Error saving data!" + e);
      }
    },

    SaveCardBasedMigration : async () => {
      await dexie.trees.update(TREE_ID, {location: "cardbased", synced: false});
      await dexie.cards.bulkPut(elmData);
      loadCardBasedDocument(TREE_ID);
    },

    CommitData: async () => {
      let timestamp = Date.now()
      dataWorker.postMessage({tag: "newSave", data: {db: userDbName, treeId: TREE_ID, elmData: elmData, timestamp: timestamp, savedImmutables: savedObjectIds}});
    },

    CommitDataResult: async () => {
      // From dataWorker, NOT ELM!

      let [ savedData
        , savedImmutables
        , conflictsExist
        , savedMetadata
      ] = elmData;

      // Add saved immutables to cache.
      savedImmutables.forEach(item => savedObjectIds.add(item));

      // Send new data to Elm
      toElm(savedData, "appMsgs", "DataSaved");

      // Mark document as clean
      DIRTY = false;

      // Maybe send metadata to Elm
      if (typeof savedMetadata !== "undefined") {
        await dexie.trees.update(TREE_ID, {updatedAt: savedMetadata.updatedAt, synced: false});
        toElm(savedMetadata, "appMsgs", "MetadataUpdate");
      }

      // Pull & Maybe push
      if (!PULL_LOCK) {
        PULL_LOCK = true;
        await data.sync(db, remoteDB, TREE_ID, conflictsExist, pullSuccessHandler, pushSuccessHandler);
        PULL_LOCK = false;
      }
    },

    PullData: async () => {
      if (!PULL_LOCK ) {
        PULL_LOCK = true;
        await data.sync(db, remoteDB, TREE_ID, null, pullSuccessHandler, pushSuccessHandler);
        PULL_LOCK = false;
      }
    },

    SaveImportedData: async () => {
      const now = Date.now();
      let savedImmutables = (await data.newSave(userDbName, elmData.metadata.docId, elmData, now, savedObjectIds))[1];

      const treeDoc = {...treeDocDefaults, id: elmData.metadata.docId, name: elmData.metadata.name, owner: email, createdAt: now, updatedAt: now};
      await dexie.trees.add(treeDoc);

      // Add saved immutables to cache.
      savedImmutables.forEach(item => savedObjectIds.add(item));

      toElm(elmData.metadata.docId, "importComplete")
    },

    SaveBulkImportedData: async () => {
      const now = Date.now();

      let localSavePromises =
        elmData.map(async commitReq => {
          await data.newSave(userDbName, commitReq.metadata.docId, commitReq, commitReq.metadata.updatedAt, savedObjectIds);
        });

      let treeDocPromises =
        elmData.map(async commitReq => {
          const treeDoc = {...treeDocDefaults, id: commitReq.metadata.docId, name: commitReq.metadata.name, owner: email, createdAt: now, updatedAt: now};
          await dexie.trees.add(treeDoc);
        });

      await Promise.all(localSavePromises.concat(treeDocPromises));

      // Push newly imported trees to remote
      elmData.map(async commitReq => {
        await data.sync(db, remoteDB, commitReq.metadata.docId, null, () => {}, pushSuccessHandler);
      });

      toElm(null, "importComplete");
    },
    // === Collaboration ===
    AddCollabRequest: () => {
      wsSend('rt:addCollab', { tr: elmData[0], c: elmData[1] }, true);
    },

    RemoveCollabRequest: () => {
      wsSend('rt:removeCollab', { tr: elmData[0], c: elmData[1] }, true);
    },

    SendCollabState: () => {
      COLLAB_STATE = elmData;
      wsSend('rt'
        , {uid: CLIENT_ID, tr: TREE_ID, m: elmData}, true);
    },


    // === DOM ===

    ScrollFullscreenCards: () => {
      helpers.scrollFullscreen(elmData);
    },

    DragStart: () => {
      draggingInternal = true;
      let cardElement = elmData.target.parentElement;
      let cardId = cardElement.id.replace(/^card-/, "");
      elmData.dataTransfer.setDragImage(cardElement, 0 , 0);
      elmData.dataTransfer.setData("text", "");
      toElm(cardId, "docMsgs", "DragStarted");
    },

    CopyToClipboard: () => {
      navigator.clipboard.writeText(elmData.content);

      let addFlashClass = function () {
        document.querySelectorAll(elmData.element).forEach((e) => {e.classList.add("flash")});
      };

      let removeFlashClass = function () {
        document.querySelectorAll(elmData.element).forEach((e) => {e.classList.remove("flash")});
      };

      addFlashClass();
      setTimeout(removeFlashClass, 200);
    },

    SelectAll: () => {
      document.getElementById(elmData).select();
    },

    FlashPrice: () => {
      let addFlashClass = function () {
        document.getElementById('price-amount').classList.add("flash-2");
      };

      let removeFlashClass = function () {
        document.getElementById('price-amount').classList.remove("flash-2");
      };

      addFlashClass();
      setTimeout(removeFlashClass, 400);
    },

    SetField: () => {
      let id = elmData[0];
      let field = elmData[1];
      window.requestAnimationFrame(() => {
        let tarea = document.getElementById("card-edit-" + id);
        tarea.value = field;
      })
    },

    SetFullscreen: () => {
      if(screenfull.isEnabled) {
        if(elmData) {
          screenfull.request().catch((e)=> console.log(e));
        } else {
          screenfull.exit();
        }
      }
    },

    PositionTourStep: () => {
      let stepNum = elmData[0];
      let refElementId = elmData[1];
      if (stepNum === 1) {
        tourStepPositionStepNum = elmData[0];
        tourStepPositionRefElementId = elmData[1];
      } else {
        setTimeout(positionTourStep, 100, stepNum, refElementId);
        setTimeout(addTourStepScrollHandler, 120, stepNum, refElementId, 2);
      }
    },

    // === UI ===
    UpdateCommits: () => {},

    HistorySlider: () => {
      const firstOpen = elmData[0];
      const delta = elmData[1];
      if (firstOpen) {
        wsSend('pullHistory', TREE_ID, false);
      }

      const timeout = document.getElementById('history-slider') ? 0 : 200;

      setTimeout(() => {
        let slider = document.getElementById('history-slider')
        if (slider != null) {
          slider.stepUp(delta);
          slider.dispatchEvent(new Event('input'));
        }
      }, timeout)
    },

    SaveUserSetting: () => {
      let key = elmData[0];
      let value = elmData[1];
      // Save to remote SQLite
      switch (key) {
        case 'language':
          wsSend('setLanguage', value, true);
          break;

        default:
          let currSessionData = getSessionData();
          currSessionData[key] = value;
          setSessionData(currSessionData, "SaveUserSetting");
          break;
      }
    },

    SetSidebarState: () => {
      let currSessionData = getSessionData();
      currSessionData.sidebarOpen = elmData;
      setSessionData(currSessionData, "SetSidebarState");
      window.requestAnimationFrame(()=>{
        sidebarWidth = document.getElementById('sidebar').clientWidth;
      });
    },

    SetFonts: () => {},

    SaveThemeSetting: () => {
      localStore.set("theme", elmData);
    },

    RequestFullscreen: () => {
      if (!document.fullscreenElement) {
        document.body.requestFullscreen();
      } else {
        document.exitFullscreen();
      }
    },

    Print: () => {
      window.print();
    },

    // === Misc ===

    IntegrationTestEvent: () => {
      if (window.Cypress) {
        switch (elmData) {
          case "ImportTextRequested":
            let files;
            if (typeof window.importTestIdx == "undefined") {
              files = [new File(["This is a test file.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "foo.txt", { type: "text/plain", })];
              window.importTestIdx = 1;
            } else if (window.importTestIdx === 1) {
              files = [new File(["Test file two.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "foo2.md", { type: "text/plain", })];
              window.importTestIdx++;
            } else if (window.importTestIdx === 2 || window.importTestIdx === 3 || window.importTestIdx === 4) {
              files =
                [ new File(["Test file three.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "bar1.txt", { type: "text/plain", })
                , new File(["Test file four.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "bar2", { type: "text/plain", })
                ];
              window.importTestIdx++;
            } else if (window.importTestIdx === 5) {
              files =
                [ new File(["Test file five.\n\nWith a paragraph break.\n!g\n\nAnd a split break."], "baz1.txt", { type: "text/plain", })
                , new File(["Test file six.\n\nWith a paragraph break.!gAnd a split break."], "baz2", { type: "text/plain", })
                ];
              window.importTestIdx++;
            }
        toElm(files, "docMsgs", "TestTextImportLoaded");
        }
      }
    },

    EmptyMessageShown: () => {},

    ShowWidget: () => {
      FreshworksWidget('open');
    },

    InitBeamer: () => {

    },

    CheckoutButtonClicked: async () => {
      let priceId = config.PRICE_DATA[elmData.currency][elmData.billing][elmData.plan];
      let userEmail = elmData.email;
      let data = await createCheckoutSession(userEmail, priceId);
      stripe.redirectToCheckout({ sessionId: data.sessionId })
    },

    SocketSend: () => {},
  };

  const params = { localStore, lastColumnScrolled, lastActivesScrolled, ticking, DIRTY }

  const cases = Object.assign(helpers.casesShared(elmData, params), casesWeb)

  try {
    cases[msg]();
  } catch (err) {
    console.error("Unexpected message from Elm : ", msg, elmData, err);
  }
};


function wsSend(msgTag, msgData, queueIfNotReady) {
  if (ws && ws.readyState === ws.OPEN) {
    ws.send(JSON.stringify({t: msgTag, d: msgData}));
  } else if (queueIfNotReady) {
    wsQueue.push([msgTag, msgData])
  }
}


/* === Database === */

const treeDocDefaults = {name: null, location: "couchdb", inviteUrl: null, collaborators: [], deletedAt: null};
const cardDefaults = {parentId: null, deleted: 0, content: "", position: 0, synced: false};

function treeDocToMetadata(tree) {
  return {docId: tree.id, name: tree.name, collaborators: tree.collaborators, createdAt: tree.createdAt, updatedAt: tree.updatedAt, _rev: null}
}

async function loadCardBasedDocument (treeId) {
  DATA_TYPE = CARD_DATA;
  if (cardDataSubscription != null) { cardDataSubscription.unsubscribe(); }
  if (historyDataSubscription != null) { historyDataSubscription.unsubscribe(); }

  // Load document-specific settings.
  localStore.db(treeId);
  let store = localStore.load();

  // Load local document data.
  let loadedCards = await dexie.cards.where("treeId").equals(treeId).toArray();
  const chk = getChk(treeId, loadedCards);
  if (loadedCards.length > 0) {
    loadedCards.localStore = store;
    toElm(loadedCards, "appMsgs", "CardDataReceived");
  }

  let firstLoad = true;

  // Setup Dexie liveQuery for local document data.
  cardDataSubscription = Dexie.liveQuery(() => dexie.cards.where("treeId").equals(treeId).toArray()).subscribe((cards) => {
    //console.log("LiveQuery update", cards);
    if (cards.length > 0) {
      // Preserve textarea field and cursor position.
      let currActive = document.activeElement;
      let currActiveId = currActive ? currActive.id : null;
      let currActivePos = currActive ? currActive.selectionStart : null;
      let currActiveContent = currActive ? currActive.value : null;

      toElm(cards, "appMsgs", "CardDataReceived");

      if (currActiveId && currActiveId.startsWith("card-edit-")) {
        // Restore textarea field and cursor position.
        requestAnimationFrame(() => {
          let newActive = document.getElementById(currActiveId);
          if (newActive) {
            newActive.focus();
            newActive.value = currActiveContent;
            newActive.selectionStart = currActivePos;
            newActive.selectionEnd = currActivePos;
          }
        });
      }


      saveBackupToImmortalDB(treeId, cards);
      if (firstLoad) {
        firstLoad = false;
        const firstCard = cards.filter(c => c.parentId === null)[0];
        setTimeout(() => {toElm(firstCard.id, "docMsgs", "InitialActivation")} , 20);
      }
    }
  });

  // Setup Dexie liveQuery for local history data, after initial pull.
  historyDataSubscription = Dexie.liveQuery(() => dexie.tree_snapshots.where("treeId").equals(treeId).toArray()).subscribe((history) => {
    if (history.length > 0) {
      const historyWithTs = history.map(h => ({
        ...h,
        ts: Number(h.snapshot.split(':')[0]),
        data: h.data !== null ? h.data.map(d => ({ ...d, deleted: 0 })) : h.data
      }));
      toElm(historyWithTs, "appMsgs", "HistoryDataReceived");
    }
  });

  // Pull data from remote
  pull(treeId, chk);
}

function getChk(treeId, cards) {
  if (cards.length > 0) {
    return cards.filter(c => c.synced).map(c => c.updatedAt).sort().reverse()[0];
  } else {
    return '0';
  }
}

function pull(treeId, chk) {
  wsSend("pull", [treeId, chk], true);
  setTimeout(() => {
    wsSend('pullHistoryMeta', treeId, true);
  }, 500)
}

function saveBackupToImmortalDB (treeId, cards) {
  const snapshot = _.chain(cards).sortBy('updatedAt').reverse().uniqBy('id').value();
  const trees = treeHelper(snapshot, null);
  const treeString = trees.map(treeToGkw).join('\n');
  if (ImmortalDB) {
    ImmortalDB.set('backup-snapshot:' + treeId, treeString);
  }
}

function treeToGkw (tree) {
  return "<gingko-card id=\""
    + tree.id
    + "\">\n\n"
    + tree.content
    + "\n\n"
    + tree.children.map(treeToGkw).join("\n\n")
    + "</gingko-card>";
}

function treeHelper (cards, parentId) {
  let children = _.chain(cards).filter(c => c.parentId === parentId).sortBy('position').value();
  return children.map(c => {
    let children = treeHelper(cards, c.id);
    return {id: c.id, content: c.content, children}
  });
}

async function loadGitLikeDocument (treeId) {
  DATA_TYPE = GIT_LIKE_DATA;
  // Load document-specific settings.
  localStore.db(treeId);
  let store = localStore.load();

  // Load local document data.
  let localExists;
  let [loadedData, savedIds] = await data.load(db, treeId);
  savedIds.forEach(item => savedObjectIds.add(item));
  if (savedIds.length !== 0) {
    localExists = true;
    loadedData.localStore = store;
    toElm(loadedData, "appMsgs", "GitDataReceived");
  } else {
    localExists = false;
  }

  // Pull data from remote
  let remoteExists;
  PULL_LOCK = true;
  try {
    let pullResult = await data.pull(db, remoteDB, treeId, "LoadDocument");

    if (pullResult !== null) {
      pullResult[1].forEach(item => savedObjectIds.add(item));
      toElm(pullResult[0], "appMsgs", "GitDataReceived");
    } else {
      remoteExists = false;
      if (!localExists && !remoteExists) {
        toElm(treeId, "appMsgs", "NotFound")
      }
    }
  } catch (e){
    console.error(e)
  } finally {
    PULL_LOCK = false;
  }

  // Activate first card
  setTimeout(() => {toElm(null, "docMsgs", "InitialActivation")}, 20);

  // Load doc list
  loadDocListAndSend("LoadDocument");
}

async function loadDocListAndSend() {
  loadingDocs = true;
  let docList = await dexie.trees.toArray().catch(e => {console.error(e); return []});
  toElm(docList.filter(d => d.deletedAt == null).map(treeDocToMetadata),  "documentListChanged");
  loadingDocs = false;
}


/* === Helper Functions === */

function my_uuid(length) {
  const chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let result = '';
  for (let i = 0; i < length; i++) {
    result += chars.charAt(Math.floor(Math.random() * chars.length));
  }
  return result;
}

function getSessionData() {
  let sessionStringRaw = localStorage.getItem(sessionStorageKey);
  if (sessionStringRaw) {
    return JSON.parse(sessionStringRaw);
  } else {
    return null;
  }
}

function setSessionData(data, source) {
  console.log("Setting session data:",source, JSON.stringify(data))
  localStorage.setItem(sessionStorageKey, JSON.stringify(data));
}

async function logout() {
  try {
    if (db) {
      await db.replicate.to(remoteDB);
      await db.destroy();
    }
    await fetch(document.location.origin + "/logout", {method: 'POST'});
    localStorage.removeItem(sessionStorageKey);

    // Encrypt local backups
    const backupKeys = Object.keys({ ...localStorage }).filter(k => k.startsWith("_immortal|backup-snapshot:")).map(k => k.slice(10));
    backupKeys.map(async (k) => {
      const val = await ImmortalDB.get(k);
      await ImmortalDB.set(k, await mycrypt.encrypt(val));
    });

    await dexie.trees.clear();
    await dexie.cards.clear();
    await dexie.tree_snapshots.clear();
    setTimeout(() => gingko.ports.userLoggedOutMsg.send(null), 0);
  } catch (err) {
    console.error(err)
  }
}

function pullSuccessHandler (pulledData) {
  if (pulledData === null) { return }

  toElm(pulledData, "appMsgs", "GitDataReceived")
}


function pushSuccessHandler (info) {
  toElm(Date.parse(info.end_time), "appMsgs", "SavedRemotely")
}

/* === DOM Events and Handlers === */

document.ondragenter = () => {
  if (!draggingInternal && !externalDrag) {
    externalDrag = true;
    toElm(null, "docMsgs", "DragExternalStarted");
  }
};
let scrollHorizontalAmount = 20;
let scrollHorizontalInterval = 15;
let scrollVerticalAmount = 20;
let scrollVerticalInterval = 15;

document.ondragleave = (ev) => {
  if(ev.relatedTarget === null) {
    // Dragged outside of window
    clearInterval(horizontalScrollInterval);
    clearInterval(verticalScrollInterval);
  }
}
// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
  // Clear autoscroll
  if (ev.type === "dragend" || ev.type === "drop") {
    clearInterval(horizontalScrollInterval);
    clearInterval(verticalScrollInterval);
    horizontalScrollInterval = null;
    verticalScrollInterval = null;
  }

  // Don't modify anything if dragging/dropping in textareas
  if (ev.target.className === "edit mousetrap") {
    return;
  }

  // Autoscroll
  if (ev.type === "dragover") {
    let path = ev.path || (ev.composedPath && ev.composedPath());

    let relX = (ev.clientX - sidebarWidth )/ (viewportWidth - sidebarWidth);
    let relY = (ev.clientY - 40) / (viewportHeight - 40); // 40 for header height

    if (relY <= 0.1) {
      //scroll column up
      let colToScroll = path.filter(x => x.classList && x.classList.contains('column'))[0];
      if(!verticalScrollInterval) {
        verticalScrollInterval = setInterval(()=>{
          colToScroll.scrollBy(0, -1*scrollVerticalAmount);
        }, scrollVerticalInterval);
      }
    } else if (relY >= 0.9) {
      let colToScroll = path.filter(x => x.classList && x.classList.contains('column'))[0];
      if(!verticalScrollInterval) {
        verticalScrollInterval = setInterval(()=>{
          colToScroll.scrollBy(0, scrollVerticalAmount);
        }, scrollVerticalInterval);
      }
    } else {
      clearInterval(verticalScrollInterval);
      verticalScrollInterval = null;
    }

    if (relX <= 0.1) {
      docElement = document.getElementById('document');
      if(!horizontalScrollInterval) {
        horizontalScrollInterval = setInterval(()=>{
          docElement.scrollBy(-1*scrollHorizontalAmount, 0);
        }, scrollHorizontalInterval);
      }
    } else if (relX >= 0.9) {
      docElement = document.getElementById('document');
      if(!horizontalScrollInterval) {
        horizontalScrollInterval = setInterval(()=>{
          docElement.scrollBy(scrollHorizontalAmount, 0);
        }, scrollHorizontalInterval);
      }
    } else {
      //stop horizontal scrolling
      clearInterval(horizontalScrollInterval);
      horizontalScrollInterval = null;
    }
  }
  if (externalDrag && ev.type === "drop") {
    externalDrag = false;
    let dropText = ev.dataTransfer.getData("text");
    if (dropText.startsWith("obsidian://open?")) {
      let url = new URL(dropText);
      let title = "# " + url.searchParams.get("file");
      toElm(title, "docMsgs", "DropExternal");
    } else {
      toElm(dropText, "docMsgs", "DropExternal");
    }
  } else if (draggingInternal && ev.type === "drop") {
    draggingInternal = false;
  }
  ev.preventDefault();
};


window.addEventListener("message", (ev) => {
  if(ev.data.hasOwnProperty("loggedin")) {
    toElm(ev.data.loggedin, "iframeLoginStateChange");
  }
});


window.onresize = () => {
  if (lastActivesScrolled) {
    debouncedScrollColumns(lastActivesScrolled);
  }
  if (lastColumnScrolled) {
    debouncedScrollHorizontal(lastColumnScrolled);
  }
  _.debounce(()=>{
    viewportWidth = document.documentElement.clientWidth;
    viewportHeight = document.documentElement.clientHeight;
  })
};

const debouncedScrollColumns = _.debounce(helpers.scrollColumns, 200);
const debouncedScrollHorizontal = _.debounce(helpers.scrollHorizontal, 200);


Mousetrap.bind(helpers.shortcuts, function (e, s) {
  switch (s) {
    case "enter":
      if (document.activeElement.nodeName === "TEXTAREA") {
        return;
      } else {
        toElm("enter","docMsgs", "Keyboard");
      }
      break;

    case "mod+c":
      let exportPreview = document.getElementById("export-preview");
      if (exportPreview !== null) {
        return;
      } else {
        toElm("mod+c","docMsgs", "Keyboard");
      }
      break;

    case "mod+v":
    case "mod+shift+v":
      if (document.activeElement.nodeName === "TEXTAREA") {
        return;
      }

      let elmTag = s === "mod+v" ? "Paste" : "PasteInto";

      navigator.clipboard.readText()
        .then(clipString => {
          try {
            let clipObj = JSON.parse(clipString);
            toElm(clipObj, "docMsgs", elmTag)
          } catch {
            toElm(clipString, "docMsgs", elmTag)
          }
        }).catch(err => {
          if (err.message.includes("denied")) {
            alert("Clipboard access denied. Click on the padlock icon in the address bar and allow clipboard access.")
          }
        });
      break;

    case "alt+0":
    case "alt+1":
    case "alt+2":
    case "alt+3":
    case "alt+4":
    case "alt+5":
    case "alt+6":
      if (document.activeElement.nodeName === "TEXTAREA") {
        let num = Number(s[s.length - 1]);
        let currentText = document.activeElement.value;
        let newText = currentText.replace(/^(#{0,6}) ?(.*)/, num === 0 ? '$2' : '#'.repeat(num) + ' $2');
        document.activeElement.value = newText;
        DIRTY = true;
        toElm(newText, "docMsgs", "FieldChanged");

        let cardElementId = document.activeElement.id.replace(/^card-edit/, "card");
        let card = document.getElementById(cardElementId);
        if (card !== null) {
          card.dataset.clonedContent = newText;
        }
      }
      break;

    default:
      toElm(s, "docMsgs", "Keyboard");
  }

  if (helpers.needOverride.includes(s)) {
    return false;
  }
});

Mousetrap.bind(["tab"], function () {
  document.execCommand("insertText", false, "  ");
  return false;
});

Mousetrap.bind(["shift+tab"], function () {
  return true;
});

/* === DOM manipulation === */

helpers.defineCustomTextarea(toElm, getDataType);

window.addEventListener("error", (err) => {
  console.log(err);
  if (
    err.message.match(/Cannot read properties of undefined \(reading 'childNodes'\)/)
    ||
    err.message.match(/Failed to execute 'removeChild' on 'Node'/)
  ) {
    alert("There may be an extension interfering with Gingko Writer.\n\nDisable your extensions and try again, or contact support");
    cleanBodyHelp();
  }
});

const cleanBodyHelp = () => {
  console.log("cleanBodyHelp")
  document.body
    .querySelectorAll(
      "[data-grammarly-shadow-root], [data-lastpass-root], [data-lastpass-icon-root]"
    )
    .forEach((el) => document.body.after(el));
};
