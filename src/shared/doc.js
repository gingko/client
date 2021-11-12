// @format
import * as data from "./data.js";
import Worker from "worker-loader!./data.worker.js";
const dataWorker = new Worker();

const _ = require("lodash");
const axios = require('axios');
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
let PULL_LOCK = false;
let DIRTY = false;
let draggingInternal = false;
let externalDrag = false;
let savedObjectIds = new Set();
const userStore = container.userStore;
const localStore = container.localStore;

const sessionStorageKey = "gingko-session-storage";

/* === Initializing App === */

initElmAndPorts();

async function initElmAndPorts() {
  let email = null;
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

async function setUserDbs(email) {
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
  PouchDB.sync(db, remoteDB, {live: true, retry: true, doc_ids: ["settings"]})
    .on('change', (ev) => {
      if (ev.direction === "pull") {
        gingko.ports.userSettingsChange.send(ev.change.docs[0]);
      }
    });

  // add docList design document
  let ddoc = {
    _id: "_design/testDocList",
    views: {
      docList: {
        map:
          "function (doc) {\n  if (/metadata/.test(doc._id)) {\n    emit(doc._id, doc);\n  }\n}",
      },
    },
    language: "javascript",
  };
  db.put(ddoc).catch(async (e) => e); // ignore conflict error

  // Sync document list with server
  PouchDB.sync(db, remoteDB, { filter: "_view", view: "testDocList/docList", include_docs: true, live: true, retry: true })
    .on('change',(change) =>{
      loadDocListAndSend(db, "sync.changes");
    })

  LogRocket.identify(email);
}


// External Scripts
const stripe = Stripe(config.STRIPE_PUBLIC_KEY);
FreshworksWidget('hide', 'launcher');


/* === Elm / JS Interop === */

function toElm(data, portName, tagName) {
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
  window.elmMessages.push({tag: msg, data: elmData});
  window.elmMessages = window.elmMessages.slice(-10);

  let cases = {
    // === SPA ===

    StoreUser: async () => {
      if (elmData == null) {
        console.log("AT StoreUser")
        try {
          await db.replicate.to(remoteDB);
          await axios.post(document.location.origin + "/logout");
          localStorage.removeItem(sessionStorageKey);
          await db.destroy();
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

    ConfirmCancelCard: () => {
      let tarea = document.getElementById("card-edit-" + elmData[0]);

      if (tarea === null) {
        console.log("tarea not found");
      } else {
        if (tarea.value === elmData[1] || confirm(elmData[2])) {
          toElm(null, "docMsgs", "CancelCardConfirmed");
        }
      }
    },

    // === Database ===

    InitDocument: async () => {
      TREE_ID = elmData;

      const now = Date.now();
      let metadata = {
        _id: elmData + "/metadata",
        docId: elmData,
        name: null,
        createdAt: now,
        updatedAt: now,
      };
      await db.put(metadata).catch(async (e) => e);

      localStore.db(db, elmData);
      let store = await localStore.load();
      toElm(store, "docMsgs", "LocalStoreLoaded");
    },

    LoadDocument : async () => {
      TREE_ID = elmData;

      // Load title
      let metadata = await data.loadMetadata(db, elmData);
      toElm(metadata, "docMsgs", "MetadataSaved")

      // Load local document data.
      let localExists;
      let [loadedData, savedIds] = await data.load(db, elmData);
      savedIds.forEach(item => savedObjectIds.add(item));
      if (savedIds.length !== 0) {
        localExists = true;
        toElm(loadedData, "docMsgs", "DataReceived");
      } else {
        localExists = false;
      }

      // Pull data from remote
      let remoteExists;
      PULL_LOCK = true;
      try {
        let pullResult = await data.pull(db, remoteDB, elmData, "LoadDocument");

        if (pullResult !== null) {
          remoteExists = true;
          pullResult[1].forEach(item => savedObjectIds.add(item));
          toElm(pullResult[0], "docMsgs", "DataReceived");
        } else {
          remoteExists = false;
          if (!localExists && !remoteExists) {
            toElm(null, "docMsgs", "NotFound")
          }
        }
      } catch (e){
        console.error(e)
      } finally {
        PULL_LOCK = false;
      }

      // Load document-specific settings.
      localStore.db(db, elmData);
      let store = await localStore.load();
      toElm(store, "docMsgs", "LocalStoreLoaded");

      // Load doc list
      loadDocListAndSend(remoteDB, "LoadDocument");

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
              toElm(null, "docMsgs", "NotFound")
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
      if (confirm("Are you sure you want to delete this document?")) {
        let docsFetch = await remoteDB.allDocs({
          startkey: elmData + "/",
          endkey: elmData + "/\ufff0",
        });

        let docsToDelete = docsFetch.rows.map((r) => {
          return { _id: r.id, _rev: r.value.rev, _deleted: true };
        });

        // Delete from local and remote DBs
        remoteDB.bulkDocs(docsToDelete);
        await db.bulkDocs(docsToDelete);
      }
    },

    RenameDocument: async () => {
      if (!renaming) { // Hack to prevent double rename attempt due to Browser.Dom.blur
        renaming = true;
        let saveRes = await data.renameDocument(db, TREE_ID, elmData);
        if (saveRes) {
          toElm(saveRes, "docMsgs", "MetadataSaved");
        }
        renaming = false;
      }
    },

    CommitData: async () => {
      let timestamp = Date.now()
      dataWorker.postMessage({tag: "newSave", data: {db: userDbName, treeId: TREE_ID, elmData: elmData, timestamp: timestamp, savedImmutables: savedObjectIds}});
    },

    CommitDataResult: async () => {
      let [ savedData
        , savedImmutables
        , conflictsExist
        , savedMetadata
      ] = elmData;

      // Add saved immutables to cache.
      savedImmutables.forEach(item => savedObjectIds.add(item));

      // Send new data to Elm
      toElm(savedData, "docMsgs", "DataSaved");

      // Mark document as clean
      DIRTY = false;

      // Maybe send metadata to Elm
      if (typeof savedMetadata !== "undefined") { toElm(savedMetadata, "docMsgs", "MetadataSaved")}

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
      let [ savedData
        , savedImmutables
        , conflictsExist
        , savedMetadata
      ] = await data.newSave(userDbName, elmData.metadata.docId, elmData, Date.now(), savedObjectIds);

      // Add saved immutables to cache.
      savedImmutables.forEach(item => savedObjectIds.add(item));

      toElm(elmData.metadata.docId, "importComplete")
    },

    SaveBulkImportedData: async () => {
      let localSavePromises =
        elmData.map(async commitReq => {
          await data.newSave(userDbName, commitReq.metadata.docId, commitReq, commitReq.metadata.updatedAt, savedObjectIds);
        });
      await Promise.all(localSavePromises);

      // Push newly imported trees to remote
      elmData.map(async commitReq => {
        await data.sync(db, remoteDB, commitReq.metadata.docId, null, () => {}, pushSuccessHandler);
      });

      toElm(null, "importComplete");
    },


    // === DOM ===

    ScrollCards: () => {
      helpers.scrollColumns(elmData);
      helpers.scrollHorizontal(elmData.columnIdx, elmData.instant);
      lastActivesScrolled = elmData;
      lastColumnScrolled = elmData.columnIdx;
      if (localStore.isReady()) {
        localStore.set('last-actives', elmData.lastActives);
      }
      window.requestAnimationFrame(()=>{
        updateFillets();
        let columns = Array.from(document.getElementsByClassName("column"));
        columns.map((c, i) => {
          c.addEventListener('scroll', () => {
            if(!ticking) {
              window.requestAnimationFrame(() => {
                updateFillets();
                ticking = false;
              })

              ticking = true;
            }
          })
        })
      });
    },

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

    CopyCurrentSubtree: () => {
      navigator.clipboard.writeText(JSON.stringify(elmData));
      let addFlashClass = function () {
        let activeCard = document.querySelectorAll(".card.active");
        let activeDescendants = document.querySelectorAll(".group.active-descendant");
        activeCard.forEach((c) => c.classList.add("flash"));
        activeDescendants.forEach((c) => c.classList.add("flash"));
      };

      let removeFlashClass = function () {
        let activeCard = document.querySelectorAll(".card.active");
        let activeDescendants = document.querySelectorAll(".group.active-descendant");
        activeCard.forEach((c) => c.classList.remove("flash"));
        activeDescendants.forEach((c) => c.classList.remove("flash"));
      };

      addFlashClass();
      setTimeout(removeFlashClass, 200);
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

    TextSurround: () => {
      let id = elmData[0];
      let surroundString = elmData[1];
      let tarea = document.getElementById("card-edit-" + id);
      let card = document.getElementById("card-" + id);

      if (tarea === null) {
        console.log("Textarea not found for TextSurround command.");
      } else {
        let start = tarea.selectionStart;
        let end = tarea.selectionEnd;
        if (start !== end) {
          let text = tarea.value.slice(start, end);
          let modifiedText = surroundString + text + surroundString;
          let newValue = tarea.value.substring(0, start) + modifiedText + tarea.value.substring(end);
          tarea.value = newValue;
          let cursorPos = start + modifiedText.length;
          tarea.setSelectionRange(cursorPos, cursorPos);
          DIRTY = true;
          toElm(newValue, "docMsgs", "FieldChanged");

          if (card !== null) {
            card.dataset.clonedContent = newValue;
          }
        }
      }
    },

    SetTextareaClone: () => {
      let id = elmData[0];
      let card = document.getElementById("card-" + id);
      if (card === null) {
        console.error("Card not found for autogrowing textarea");
      } else {
        card.dataset.clonedContent = elmData[1];
      }
    },

    SetField: () => {
      let id = elmData[0];
      let field = elmData[1];
      window.requestAnimationFrame(() => {
        let tarea = document.getElementById("card-edit-" + id);
        tarea.value = field;
      })
    },

    SetCursorPosition: () => {
      let pos = elmData[0];
      setTimeout(() => document.activeElement.setSelectionRange(pos, pos), 0);
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

    EmptyMessageShown: () => {},

    ShowWidget: () => {
      FreshworksWidget('open');
    },

    CheckoutButtonClicked: async () => {
      let priceId = config.PRICE_DATA[elmData.currency][elmData.billing][elmData.plan];
      let userEmail = elmData.email;
      let data = await createCheckoutSession(userEmail, priceId);
      let checkoutResult = stripe.redirectToCheckout({sessionId: data.sessionId});
    },

    SocketSend: () => {},

    ConsoleLogRequested: () => console.error(elmData),
  };

  try {
    cases[msg]();
  } catch (err) {
    console.error("Unexpected message from Elm : ", msg, elmData, err);
  }
};




/* === Database === */

async function loadDocListAndSend(dbToLoadFrom, source) {
  let docList = await data.getDocumentList(dbToLoadFrom);
  toElm(docList, "documentListChanged");
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
  toElm(pulledData, "docMsgs", "DataReceived")
}


function pushSuccessHandler (info) {
  toElm(Date.parse(info.end_time), "docMsgs", "SavedRemotely")
}

/* === DOM Events and Handlers === */

document.ondragenter = (ev) => {
  if (!draggingInternal && !externalDrag) {
    externalDrag = true;
    toElm(null, "docMsgs", "DragExternalStarted");
  }
};
// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
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
  if (ev.target.className !== "edit mousetrap") {
    ev.preventDefault();
  }
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
};

const updateFillets = () => {
  let columns = Array.from(document.getElementsByClassName("column"));
  let filletData = helpers.getFilletData(columns);
  columns.map((c,i) => {
    helpers.setColumnFillets(c,i, filletData);
  })
}

const debouncedScrollColumns = _.debounce(helpers.scrollColumns, 200);
const debouncedScrollHorizontal = _.debounce(helpers.scrollHorizontal, 200);

const selectionHandler = function () {
  if (document.activeElement.nodeName == "TEXTAREA") {
    let {
      selectionStart,
      selectionEnd,
      selectionDirection,
    } = document.activeElement;
    let length = document.activeElement.value.length;
    let [before, after] = [
      document.activeElement.value.substring(0, selectionStart),
      document.activeElement.value.substring(selectionStart),
    ];
    let cursorPosition = "other";

    if (length == 0) {
      cursorPosition = "empty";
    } else if (selectionStart == 0 && selectionEnd == 0) {
      cursorPosition = "start";
    } else if (selectionStart == length && selectionEnd == length) {
      cursorPosition = "end";
    } else if (selectionStart == 0 && selectionDirection == "backward") {
      cursorPosition = "start";
    } else if (selectionEnd == length && selectionDirection == "forward") {
      cursorPosition = "end";
    }

    toElm(
      {
        selected: selectionStart !== selectionEnd,
        position: cursorPosition,
        text: [before, after],
      },
      "docMsgs",
      "TextCursor"
    );
  }
};

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


const positionTourStep = function (stepNum, refElementId) {
  let refElement;
  if (stepNum == 1 || stepNum == 5) {
    refElement = document.getElementById(refElementId);
  } else if (stepNum == 2) {
    refElement = document.getElementById(refElementId).parentElement.parentElement.parentElement.getElementsByClassName('ins-right')[0];
  }
  let tourElement = document.getElementById("welcome-step-"+stepNum);
  if (refElementId !== null && tourElement !== null) {
    let refRect = refElement.getBoundingClientRect();
    tourElement.style.top = refRect.top + "px";
    tourElement.style.left = refRect.left + "px";
  }
}

const addTourStepScrollHandler = (stepNum, refElementId, colNum) => {
  let col = document.getElementsByClassName("column")[colNum-1];
  if (col) {
    let ticking =  false;
    col.onscroll = () => {
      if (!ticking) {
        window.requestAnimationFrame(()=> {
          positionTourStep(stepNum, refElementId);
          ticking = false;
        })

        ticking = true;
      }
    }
  }
}

const observer = new MutationObserver(function (mutations) {
  const isTextarea = function (node) {
    return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap";
  };

  const isTourStepOne = function (node) {
    return document.getElementById('welcome-step-1');
  };

  let textareas = [];

  mutations.map((m) => {
    [].slice.call(m.addedNodes).map((n) => {
      if (isTextarea(n)) {
        textareas.push(n);
      } else if (isTourStepOne(n)) {
        // Add handlers with MutationObserver for first step
        positionTourStep(tourStepPositionStepNum, tourStepPositionRefElementId);
        addTourStepScrollHandler(tourStepPositionStepNum, tourStepPositionRefElementId, 2)
      } else {
        if (n.querySelectorAll) {
          let tareas = [].slice.call(n.querySelectorAll("textarea.edit"));
          textareas = textareas.concat(tareas);
        }
      }
    });

    [].slice.call(m.removedNodes).map((n) => {
      if ("getElementsByClassName" in n && n.getElementsByClassName("edit mousetrap").length != 0) {
        updateFillets();
      }
    })
  });

  if (textareas.length !== 0) {
    textareas.map((t) => {
      t.onkeyup = selectionHandler;
      t.onclick = selectionHandler;
      t.onfocus = selectionHandler;
    });
    if (document.getElementById("app-fullscreen") === null) {
      window.addEventListener('click', editBlurHandler)
    }
  } else {
    window.removeEventListener('click', editBlurHandler);
  }
});

const editBlurHandler = (ev) => {
  let targetClasses = ev.target.classList;
  if (ev.target.nodeName == "SPAN" && targetClasses.contains("card-btn") && targetClasses.contains("save")) {
    return;
  } else if (isEditTextarea(ev.target)) {
    return;
  } else {
    if(!(isEditTextarea(document.activeElement))) {
      toElm(null, "docMsgs", "ClickedOutsideCard");
    }
  }
};

const isEditTextarea = (node) => {
  return node.nodeName == "TEXTAREA" && node.classList.contains("edit") && node.classList.contains("mousetrap");
}

const observerConfig = { childList: true, subtree: true };

observer.observe(document.body, observerConfig);
