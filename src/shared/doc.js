// @format
import * as data from "./data.js";

const jQuery = require("jquery");
const _ = require("lodash");
const axios = require('axios');
require("textarea-autosize");
const Mousetrap = require("mousetrap");
const container = require("Container");
const config = require("../../config.js");
require("../shared/GitGraph.js");

import PouchDB from "pouchdb";

const helpers = require("./doc-helpers");
import { Elm } from "../elm/Main";

/* === Global Variables === */

let lastActivesScrolled = null;
let lastColumnScrolled = null;
let lang = "en";
let helpVisible;
let helpWidgetLauncher;
window.elmMessages = [];

let remoteDB;
let db;
let gingko;
let TREE_ID;
let PULL_LOCK = false;
let DIRTY = false;
let savedObjectIds = [];
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
    setUserDbs(sessionData.email);

    // Load user settings
    try {
      settings = await userStore.load();
    } catch (e) {
      console.log("failed", e)
    }
  }


  settings.email = email;
  settings.seed = Date.now();
  lang = settings.language || "en";
  console.log("loaded settings" ,settings)

  gingko = Elm.Main.init({
    node: document.getElementById("elm"),
    flags: settings,
  });

  // All messages from Elm
  gingko.ports.infoForOutside.subscribe(function (elmdata) {
    fromElm(elmdata.tag, elmdata.data);
  });

  window.checkboxClicked = (cardId, number) => {
    toElm([cardId, number], "docMsgs", "CheckboxClicked");
  };

  // Whenever localStorage changes in another tab, report it if necessary.
  window.addEventListener(
    "storage",
    function (event) {
      if (
        event.storageArea === localStorage &&
        event.key === sessionStorageKey
      ) {
        gingko.ports.userStateChanged.send(event.newValue);
      }
    },
    false
  );

  // Prevent closing if unsaved changes exist.
  window.addEventListener('beforeunload', (event) => {
    if (DIRTY) {
      event.preventDefault();
      event.returnValue = '';
    }
  });
}

function setUserDbs(email) {
  console.log("Inside setUserDbs", email, helpers.toHex(email));
  const userDbName = `userdb-${helpers.toHex(email)}`;
  let userDbUrl = config.COUCHDB_SERVER + "/" + userDbName;
  var remoteOpts = { skip_setup: true };
  remoteDB = new PouchDB(userDbUrl, remoteOpts);
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
}


const stripe = Stripe(config.STRIPE_PUBLIC_KEY);


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
          JSON.stringify(_.omit(elmData, "seed"))
        );
        setUserDbs(elmData.email);
        elmData.seed = Date.now();
        setTimeout(() => gingko.ports.userLoginChange.send(elmData), 0);
      }
    },

    // === Dialogs, Menus, Window State ===

    Alert: () => {
      alert(elmData);
    },

    SetDirty: () => {
      DIRTY = true;
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
      savedObjectIds = savedObjectIds.concat(savedIds);
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
          savedObjectIds = savedObjectIds.concat(pullResult[1]);
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

    SaveData: async () => {
      let [ savedData
          , savedImmutables
          , conflictsExist
          , savedMetadata
          ] = await data.saveData(db, TREE_ID, elmData, savedObjectIds);

      // Add saved immutables to cache.
      savedObjectIds = savedObjectIds.concat(savedImmutables);

      // Send new revs to Elm
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
      let isBulk = Array.isArray(elmData);
      if (!isBulk) {
        let now = Date.now();
        elmData.metadata.createdAt = now;
        elmData.metadata.updatedAt = now;
      }

      let dataArray = isBulk ? elmData : [elmData];
      let savePromises = dataArray.map((doc) => {
        let dataRows = [
          ...doc.data.commits,
          ...doc.data.treeObjects,
          ...doc.data.refs,
          doc.metadata,
        ];
        let toSave = dataRows.map((r) => {
          r._id = doc.id + "/" + r._id;
          return _.omit(r, "_rev");
        });
        return remoteDB.bulkDocs(toSave);
      });
      await Promise.all(savePromises);

      if (isBulk) {
        toElm(null, "importComplete");
      } else {
        toElm(elmData.metadata.docId, "importComplete")
      }
    },


    // === DOM ===

    ScrollCards: () => {
      helpers.scrollColumns(elmData);
      helpers.scrollHorizontal(elmData.columnIdx, elmData.instant);
      lastActivesScrolled = elmData;
      lastColumnScrolled = elmData.columnIdx;
      localStore.set('last-actives', elmData.lastActives);
    },

    ScrollFullscreenCards: () => {
      helpers.scrollFullscreen(elmData);
    },

    DragStart: () => {
      elmData.dataTransfer.setData("text", "");
      toElm(elmData.target.id.replace(/^card-/, ""), "docMsgs", "DragStarted");
    },

    CopyCurrentSubtree: () => {
      navigator.clipboard.writeText(JSON.stringify(elmData));
      let addFlashClass = function () {
        jQuery(".card.active").addClass("flash");
        jQuery(".group.active-descendant").addClass("flash");
      };

      let removeFlashClass = function () {
        jQuery(".card.active").removeClass("flash");
        jQuery(".group.active-descendant").removeClass("flash");
      };

      addFlashClass();
      setTimeout(removeFlashClass, 200);
    },

    TextSurround: () => {
      let id = elmData[0];
      let surroundString = elmData[1];
      let tarea = document.getElementById("card-edit-" + id);

      if (tarea === null) {
        console.log("Textarea not found for TextSurround command.");
      } else {
        let start = tarea.selectionStart;
        let end = tarea.selectionEnd;
        if (start !== end) {
          let text = tarea.value.slice(start, end);
          let modifiedText = surroundString + text + surroundString;
          document.execCommand("insertText", true, modifiedText);
        }
      }
    },

    SetCursorPosition: () => {
      let pos = elmData[0];
      setTimeout(() => document.activeElement.setSelectionRange(pos, pos), 0);
    },

    SetFullscreen: () => {
      if (elmData && !document.fullscreenElement) {
        document.documentElement.requestFullscreen();
      } else if (!elmData && document.fullscreenElement) {
        document.exitFullscreen();
      }
    },

    // === UI ===
    UpdateCommits: () => {},

    SetVideoModal: () => {
      userStore.set("video-modal-is-open", elmData);
    },

    SetLanguage: () => {
      lang = elmData;
      userStore.set("language", elmData);
    },

    SetFonts: () => {},

    SaveThemeSetting: () => {
      localStore.set("theme", elmData);
    },

    SetShortcutTray: () => {
      userStore.set("shortcutTrayOpen", elmData);
    },

    // === Misc ===

    EmptyMessageShown: () => {},

    CheckoutButtonClicked: async () => {
      let data = await createCheckoutSession(elmData);
      let checkoutResult = stripe.redirectToCheckout({sessionId: data.sessionId});
      console.log(checkoutResult);
    },

    TriggerMailto: () => {
      // Hack to avoid committing personal email address:
      let mail = document.createElement("a");
      mail.href = `mailto:${config.SUPPORT_EMAIL}`;
      mail.target = "_blank";
      mail.click();
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

var createCheckoutSession = function(priceId) {
  return fetch("/create-checkout-session", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({
      priceId: priceId
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

// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
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
};

const debouncedScrollColumns = _.debounce(helpers.scrollColumns, 200);
const debouncedScrollHorizontal = _.debounce(helpers.scrollHorizontal, 200);

const editingInputHandler = function (ev) {
  DIRTY = true;
  toElm(ev.target.value, "docMsgs", "FieldChanged");
  selectionHandler(ev);
};

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

document.onselectionchange = selectionHandler;

Mousetrap.bind(helpers.shortcuts, function (e, s) {
  if (s === "mod+v" || s === "mod+shift+v") {
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
  } else {
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

const observer = new MutationObserver(function (mutations) {
  const isTextarea = function (node) {
    return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap";
  };

  const isHelpWidget = function (node) {
    return node.nodeName == "IFRAME" && node.id == "launcher-frame";
  };

  let textareas = [];

  mutations.map((m) => {
    [].slice.call(m.addedNodes).map((n) => {
      if (isTextarea(n)) {
        textareas.push(n);
      } else if (isHelpWidget(n)) {
        helpWidgetLauncher = n;
        if (!helpVisible) {
          helpWidgetLauncher.style.visibility = "hidden";
        }
      } else {
        if (n.querySelectorAll) {
          let tareas = [].slice.call(n.querySelectorAll("textarea.edit"));
          textareas = textareas.concat(tareas);
        }
      }
    });
  });

  if (textareas.length !== 0) {
    textareas.map((t) => {
      t.oninput = editingInputHandler;
    });

    jQuery(textareas).textareaAutoSize();
  }
});

const observerConfig = { childList: true, subtree: true };

observer.observe(document.body, observerConfig);
