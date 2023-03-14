// @format
import * as data from "./data.js";
import Worker from "worker-loader!./data.worker.js";
import hlc from '@tpp/hybrid-logical-clock';
import uuid from '@tpp/simple-uuid';
const dataWorker = new Worker();

const _ = require("lodash");
const Mousetrap = require("mousetrap");
const screenfull = require("screenfull");
const container = require("Container");
const platform = require("platform");
const config = require("../../config.js");

import LogRocket from 'logrocket';
if(window.location.origin === config.PRODUCTION_SERVER) {
  LogRocket.init(config.LOGROCKET_APPID);
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
dexie.version(3).stores({
  trees: "id,updatedAt",
  cards: "updatedAt,treeId",
  tree_snapshots: "snapshot, treeId"
});

const helpers = require("./doc-helpers");
import { Elm } from "../elm/Main";

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
let userDbName;
let email = null;
let ws;
let PULL_LOCK = false;
let DIRTY = false;
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
const userStore = container.userStore;
const localStore = container.localStore;

const sessionStorageKey = "gingko-session-storage";

/* === Initializing App === */

initElmAndPorts();

async function initElmAndPorts() {
  let sessionData = localStorage.getItem(sessionStorageKey) || null;
  let settings = {language: "en"};
  if (sessionData) {
    console.log("sessionData found", sessionData);
    sessionData = JSON.parse(sessionData);
    email = sessionData.email;
    await setUserDbs(sessionData.email);
    // Load user settings
    try {
      settings = await userStore.load();
    } catch (e) {
      console.log("failed", e)
    }
    settings.sidebarOpen = (sessionData.hasOwnProperty('sidebarOpen')) ?  sessionData.sidebarOpen : false;
    sidebarWidth = settings.sidebarOpen ? 215 : 40;
  }


  let timestamp = Date.now();
  settings.email = email;
  settings.seed = timestamp;
  settings.isMac = platform.os.family === 'OS X';
  settings.currentTime = timestamp;
  settings.fromLegacy = document.referrer.startsWith(config.LEGACY_URL);
  lang = settings.language || "en";

  gingko = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: settings,
  });

  // All messages from Elm
  gingko.ports.infoForOutside.subscribe(function (elmdata) {
    fromElm(elmdata.tag, elmdata.data);
  });

  // Messages from dataWorker
  dataWorker.onmessage = (e) => {
    fromElm(e.data.tag, e.data.data);
  };

  window.checkboxClicked = (cardId, number) => {
    toElm([cardId, number], "docMsgs", "CheckboxClicked");
  };

  // Prevent closing if unsaved changes exist.
  window.addEventListener('beforeunload', (event) => {
    if (DIRTY) {
      event.preventDefault();
      event.returnValue = '';
    }
  });

  // Fullscreen change event
  // This is so that we can use "Esc" once to leave fullscreen mode.
  if (screenfull.isEnabled) {
    screenfull.on('change', () => {
      toElm(screenfull.isFullscreen, "docMsgs", "FullscreenChanged")
    });
  }

  window.addEventListener('beforeprint', (event) => {
    toElm(null, "docMsgs", "WillPrint");
  })
}

async function setUserDbs(eml) {
  email = eml;
  console.log("Inside setUserDbs", email, helpers.toHex(email));
  userDbName = `userdb-${helpers.toHex(email)}`;
  let userDbUrl = window.location.origin + "/db/" + userDbName;
  var remoteOpts = { skip_setup: true };
  remoteDB = new PouchDB(userDbUrl, remoteOpts);
  // Check remoteDB exists and accessible before continuing
  let remoteDBinfo = await remoteDB.info().catch((e) => e);
  if (remoteDBinfo.error === "unauthorized") {
    //remove localStorage session redirect to login
    localStorage.removeItem(sessionStorageKey);
    alert("Your session expired.\nClick OK to login again");
    document.location = document.location.origin + '/login';
  }

  db = new PouchDB(userDbName);
  userStore.db(db, remoteDB);


  // Sync user settings

  await PouchDB.replicate(remoteDB, db, {retry: true, doc_ids: ["settings"]});
  PouchDB.sync(db, remoteDB, {live: true, retry: true, doc_ids: ["settings"]})
    .on('change', (ev) => {
      if (ev.direction === "pull") {
        gingko.ports.userSettingsChange.send(ev.change.docs[0]);
      }
    });

  ws = new WebSocket(window.location.origin.replace('http','ws'));

  ws.onopen = () => {
    console.log('connected');
  }

  ws.onmessage = async (e) => {
    const data = JSON.parse(e.data);
    try {
      switch (data.t) {
        case 'cards':
          await dexie.cards.bulkPut(data.d.map(c => ({...c, synced: true})));
          break;

        case 'pushOk':
          hlc.recv(data.d);
          toElm(data, "appMsgs", "PushOk");
          break;

        case 'trees':
          await dexie.trees.bulkPut(data.d.map(t => ({...t, synced : true})));
          break;

        case 'treesOk':
          await dexie.trees.where('updatedAt').belowOrEqual(data.d).modify({synced: true});
          break;

        case 'historyMeta': {
          const {tr, d} = data;
          const snapshotData = d.map(hmd => ({snapshot: hmd.id , treeId: tr, data: null}));
          try {
            await dexie.tree_snapshots.bulkAdd(snapshotData);
          } catch (e) {
            const errorNames = e.failures.map(f => f.name);
            if (errorNames.every(n => n === 'ConstraintError')) {
              // Ignore
            } else {
              throw e;
            }
          }
          break;
        }

        case 'history': {
          const {tr, d} = data;
          const snapshotData = d.map(hd => ({snapshot: hd.id , treeId: tr, data: hd.d.map(d => ({...d, synced: true}))}));
          await dexie.tree_snapshots.bulkPut(snapshotData);
          break;
        }
      }
    } catch (e) {
      console.log(e);
    }
  }

  // Sync document list with server

  let firstLoad = true;

  Dexie.liveQuery(() => dexie.trees.toArray()).subscribe((trees) => {
    const docMetadatas = trees.filter(t => t.deletedAt == null).map(treeDocToMetadata);
    if (!loadingDocs && !firstLoad) {
      toElm(docMetadatas, "documentListChanged");
    }

    const unsyncedTrees = trees.filter(t => !t.synced);
    if (unsyncedTrees.length > 0) {
      ws.send(JSON.stringify({t: 'trees', d: unsyncedTrees}));
    }
    firstLoad = false;
  });

  LogRocket.identify(email);

  if (email !== "cypress@testing.com") {
    self.fwSettings={
      'widget_id': config.FRESHDESK_APPID
    };
    !function(){if("function"!=typeof window.FreshworksWidget){var n=function(){n.q.push(arguments)};n.q=[],window.FreshworksWidget=n}}()
    let freshdeskScript = document.createElement('script');
    freshdeskScript.setAttribute('src', `https://euc-widget.freshworks.com/widgets/${config.FRESHDESK_APPID}.js`)
    freshdeskScript.setAttribute('async','');
    freshdeskScript.setAttribute('defer','');
    document.head.appendChild(freshdeskScript);
    FreshworksWidget('hide', 'launcher');
  }

  if(window.location.origin === config.PRODUCTION_SERVER) {
    self.beamer_config = {
      product_id: config.BEAMER_APPID,
      selector: "#notifications-icon",
      user_id: email,
      user_email: email
    };
    let beamerScript = document.createElement('script');
    beamerScript.setAttribute('src', 'https://app.getbeamer.com/js/beamer-embed.js');
    beamerScript.setAttribute('defer', 'defer');
    document.head.appendChild(beamerScript);
  }
}


// External Scripts
const stripe = Stripe(config.STRIPE_PUBLIC_KEY);


/* === Elm / JS Interop === */

function toElm(data, portName, tagName) {
  console.log("toElm", portName, tagName, data);
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
  console.log("fromElm", msg, elmData);
  window.elmMessages.push({tag: msg, data: elmData});
  window.elmMessages = window.elmMessages.slice(-10);

  let casesWeb = {
    // === SPA ===

    StoreUser: async () => {
      if (elmData == null) {
        console.log("AT StoreUser")
        try {
          await db.replicate.to(remoteDB);
          await fetch(document.location.origin + "/logout", {method: 'POST'});
          localStorage.removeItem(sessionStorageKey);
          await db.destroy();
          await dexie.trees.clear();
          setTimeout(() => gingko.ports.userLoginChange.send(null), 0);
        } catch (err) {
          console.error(err)
        }
      } else {
        localStorage.setItem(
          sessionStorageKey,
          JSON.stringify(_.pick(elmData, ["email","language"]))
        );
        await setUserDbs(elmData.email);
        elmData.seed = Date.now();
        setTimeout(() => gingko.ports.userLoginChange.send(elmData), 0);
      }
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
      const treeDoc = {...treeDocDefaults, id: TREE_ID, owner: email, createdAt: now, updatedAt: now};

      await dexie.trees.add(treeDoc);

      // Set localStore db
      localStore.db(elmData);
    },

    LoadDocument : async () => {
      TREE_ID = elmData;

      // Load title
      const treeDoc = await dexie.trees.get(elmData);
      if (treeDoc) {
        toElm(treeDocToMetadata(treeDoc), "appMsgs", "MetadataUpdate")
      } else {
        toElm(null, "appMsgs", "NotFound")
        return;
      }

      if (treeDoc.location == "couchdb") {
        loadGitLikeDocument(elmData);
      } else if (treeDoc.location == "cardbased") {
        loadCardBasedDocument(elmData);
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
        localExists = true;
        toElm([metadata.name, loadedData], "copyLoaded");
      } else {
        localExists = false;
        let remoteExists;
        PULL_LOCK = true;
        try {
          let pullResult = await data.pull(db, remoteDB, elmData, "LoadDocument");

          if (pullResult !== null) {
            remoteExists = true;
            pullResult[1].forEach(item => savedObjectIds.add(item));
            toElm([metadata.name, pullResult[0]], "copyLoaded");
          } else {
            remoteExists = false;
            if (!localExists && !remoteExists) {
              toElm(null, "appMsgs", "NotFound")
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
      loadDocListAndSend(remoteDB, "GetDocumentList");
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
      if (ws.readyState == ws.OPEN && ws.bufferedAmount == 0 && elmData.dlts.length > 0) {
        ws.send(JSON.stringify({t: 'push', d: elmData}));
      }
    },

    SaveCardBased : async () => {
      if (elmData !== null) {
        let newData = elmData.toAdd.map((c) => { return { ...c, updatedAt: hlc.nxt() }})
        const toMarkSynced = elmData.toMarkSynced.map((c) => { return { ...c, synced: true }})

        let toMarkDeleted = [];
        if (elmData.toMarkDeleted.length > 0) {
          const timestamp = Date.now();
          const deleteHash = uuid();
          toMarkDeleted = elmData.toMarkDeleted.map((c, i) => ({ ...c, updatedAt: `${timestamp}:${i}:${deleteHash}` }));
        }

        dexie.transaction('rw', dexie.cards, async () => {
          dexie.cards.bulkPut(newData.concat(toMarkSynced).concat(toMarkDeleted));
          dexie.cards.bulkDelete(elmData.toRemove);
        }).catch((e) => {
          alert("Error saving data!" + e);
        });
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
      let [ savedData
        , savedImmutables
        , conflictsExist
        , savedMetadata
      ] = await data.newSave(userDbName, elmData.metadata.docId, elmData, now, savedObjectIds);

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
      if (stepNum == 1) {
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
      // History opened
      // TODO: Check if we're actually in a card-based document
      if (ws.readyState == ws.OPEN && ws.bufferedAmount == 0) {
        ws.send(JSON.stringify({t: "pullHistory", d: TREE_ID}));
      }

      window.requestAnimationFrame(() => {
        let slider = document.getElementById('history-slider')
        if (slider != null) {
          slider.stepUp(elmData);
          slider.dispatchEvent(new Event('input'));
        }
      })
    },

    SaveUserSetting: () => {
      let key = elmData[0];
      let value = elmData[1];
      switch(key) {
        case "language":
          lang = value;
          userStore.set("language", value);
          break;

        default:
          userStore.set(key, value);
          break;
      }
    },

    SetSidebarState: () => {
      let currSessionData = JSON.parse(localStorage.getItem(sessionStorageKey));
      currSessionData.sidebarOpen = elmData;
      localStorage.setItem(sessionStorageKey, JSON.stringify(currSessionData));
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

    SetShortcutTray: () => {
      userStore.set("shortcutTrayOpen", elmData);
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
            } else if (window.importTestIdx == 1) {
              files = [new File(["Test file two.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "foo2.md", { type: "text/plain", })];
              window.importTestIdx++;
            } else if (window.importTestIdx == 2 || window.importTestIdx == 3 || window.importTestIdx == 4) {
              files =
                [ new File(["Test file three.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "bar1.txt", { type: "text/plain", })
                , new File(["Test file four.\n\nWith a paragraph break.\n\n---\n\nAnd a split break."], "bar2", { type: "text/plain", })
                ];
              window.importTestIdx++;
            } else if (window.importTestIdx == 5) {
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
      let checkoutResult = stripe.redirectToCheckout({sessionId: data.sessionId});
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




/* === Database === */

const treeDocDefaults = {name: null, location: "couchdb", inviteUrl: null, collaborators: "[]", deletedAt: null};

function treeDocToMetadata(tree) {
  return {docId: tree.id, name: tree.name, createdAt: tree.createdAt, updatedAt: tree.updatedAt, _rev: null}
}

async function loadCardBasedDocument (treeId) {
  // Load local document data.
  let chk;
  let loadedCards = await dexie.cards.where("treeId").equals(treeId).toArray();
  if (loadedCards.length > 0) {
    chk = loadedCards.filter(c => c.synced).map(c => c.updatedAt).sort().reverse()[0];
    toElm(loadedCards, "appMsgs", "CardDataReceived");
  } else {
    chk = '0';
  }

  // Setup Dexie liveQuery for local document data.
  Dexie.liveQuery(() => dexie.cards.where("treeId").equals(treeId).toArray()).subscribe((cards) => {
    console.log("LiveQuery update", cards);
    toElm(cards, "appMsgs", "CardDataReceived");
    saveBackupToImmortalDB(treeId, cards);
  });

  // Setup Dexie liveQuery for local history data, after initial pull.
  Dexie.liveQuery(() => dexie.tree_snapshots.where("treeId").equals(treeId).toArray()).subscribe((history) => {
    const historyWithTs = history.map(h => ({...h, ts: Number(h.snapshot.split(':')[0]), data: h.data.map(d => ({...d, deleted: 0}))}));
    toElm(historyWithTs, "appMsgs", "HistoryDataReceived");
  });

  // Pull data from remote
  if (ws.readyState == ws.OPEN && ws.bufferedAmount == 0) {
    ws.send(JSON.stringify({t: "pull", d: [treeId, chk]}));
    setTimeout(() => {
      ws.send(JSON.stringify({t: "pullHistoryMeta", d: treeId}));
    }, 500);
  }
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
  let children = _.chain(cards).filter(c => c.parentId == parentId).sortBy('position').value();
  return children.map(c => {
    let children = treeHelper(cards, c.id);
    return {id: c.id, content: c.content, children}
  });
}

async function loadGitLikeDocument (treeId) {
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
      remoteExists = true;
      pullResult[1].forEach(item => savedObjectIds.add(item));
      toElm(pullResult[0], "appMsgs", "GitDataReceived");
    } else {
      remoteExists = false;
      if (!localExists && !remoteExists) {
        toElm(null, "appMsgs", "NotFound")
      }
    }
  } catch (e){
    console.error(e)
  } finally {
    PULL_LOCK = false;
  }

  // Load doc list
  loadDocListAndSend(remoteDB, "LoadDocument");
}

async function loadDocListAndSend(dbToLoadFrom, source) {
  loadingDocs = true;
  let docList = await dexie.trees.toArray();
  toElm(docList.filter(d => d.deletedAt == null).map(treeDocToMetadata),  "documentListChanged");
  loadingDocs = false;
}

/* === Stripe === */

var createCheckoutSession = function(userEmail, priceId) {
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
};


/* === Helper Functions === */

function prefix(id) {
  return TREE_ID + "/" + id;
}

function unprefix(id) {
  return id.slice(TREE_ID.length + 1);
}


function pullSuccessHandler (pulledData) {
  if (pulledData === null) { return }

  toElm(pulledData, "appMsgs", "GitDataReceived")
}


function pushSuccessHandler (info) {
  toElm(Date.parse(info.end_time), "appMsgs", "SavedRemotely")
}

/* === DOM Events and Handlers === */

document.ondragenter = (ev) => {
  if (!draggingInternal && !externalDrag) {
    externalDrag = true;
    toElm(null, "docMsgs", "DragExternalStarted");
  }
};
let scrollHorizontalAmount = 20;
let scrollHorizontalInterval = 15;
let scrollVerticalAmount = 20;
let scrollVerticalInterval = 15;
// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
  // Clear autoscroll
  if (ev.type == "dragend" || ev.type == "drop") {
    clearInterval(horizontalScrollInterval);
    clearInterval(verticalScrollInterval);
    horizontalScrollInterval = null;
    verticalScrollInterval = null;
  }

  // Don't modify anything if dragging/dropping in textareas
  if (ev.target.className == "edit mousetrap") {
    return;
  }

  // Autoscroll
  if (ev.type == "dragover") {
    let relX = (ev.clientX - sidebarWidth )/ (viewportWidth - sidebarWidth);
    let relY = (ev.clientY - 40) / (viewportHeight - 40); // 40 for header height

    if (relY <= 0.1) {
      //scroll column up
      let colToScroll = ev.path.filter(x => x.classList && x.classList.contains('column'))[0];
      if(!verticalScrollInterval) {
        verticalScrollInterval = setInterval(()=>{
          colToScroll.scrollBy(0, -1*scrollVerticalAmount);
        }, scrollVerticalInterval);
      }
    } else if (relY >= 0.9) {
      let colToScroll = ev.path.filter(x => x.classList && x.classList.contains('column'))[0];
      if(!verticalScrollInterval) {
        verticalScrollInterval = setInterval(()=>{
          colToScroll.scrollBy(0, 1*scrollVerticalAmount);
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
  if (externalDrag && ev.type == "drop") {
    externalDrag = false;
    let dropText = ev.dataTransfer.getData("text");
    if (dropText.startsWith("obsidian://open?")) {
      let url = new URL(dropText);
      let title = "# " + url.searchParams.get("file");
      toElm(title, "docMsgs", "DropExternal");
    } else {
      toElm(dropText, "docMsgs", "DropExternal");
    }
  } else if (draggingInternal && ev.type == "drop") {
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
      if (document.activeElement.nodeName == "TEXTAREA") {
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
      let elmTag = s === "mod+v" ? "Paste" : "PasteInto";

      navigator.clipboard.readText()
        .then(clipString => {
          try {
            let clipObj = JSON.parse(clipString);
            toElm(clipObj, "docMsgs", elmTag)
          } catch {
            toElm(clipString, "docMsgs", elmTag)
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
      if (document.activeElement.nodeName == "TEXTAREA") {
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


const observer = helpers.getObserver(toElm);

const observerConfig = { childList: true, subtree: true };

observer.observe(document.body, observerConfig);
