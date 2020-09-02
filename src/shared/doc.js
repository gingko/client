const jQuery = require("jquery");
const _ = require("lodash");
const axios = require("axios");
require("textarea-autosize");
const Mousetrap = require("mousetrap");
const container = require("Container");
const config = require("../../config.js");
require("../shared/GitGraph.js");

import PouchDB from "pouchdb";


const helpers = require("./doc-helpers");
const errorAlert = helpers.errorAlert;
const { tr } = require("../shared/translation.js");
import { Elm } from "../elm/Main";



/* === Global Variables === */

var lastActivesScrolled = null;
var lastColumnScrolled = null;
var _lastFormat = null;
var _lastSelection = null;
var collab = {};
var lang;
var helpVisible;
var helpWidgetLauncher;

var remoteDB;
var gingko;
var TREE_ID;
var savedObjectIds = [];
const userStore = container.userStore;
const localStore = container.localStore;


const sessionStorageKey = "gingko-session-storage";

const ActionOnData =
  { Exit: "Exit"
  , Save: "Save"
  , SaveAs: "SaveAs"
  };
var actionOnData = ActionOnData.Save;


/* === Initializing App === */

initElmAndPorts();

async function initElmAndPorts() {
  const sessionData = localStorage.getItem(sessionStorageKey) || null;
  if (sessionData) {
    setUserDbs(sessionData);
  }

  loadUserStore();

  const initFlags = { session : sessionData, language: "en" };

  gingko = Elm.Main.init({ node: document.getElementById("elm"), flags: initFlags});

  // Page.Doc messages
  gingko.ports.infoForOutside.subscribe(function(elmdata) {
    update(elmdata.tag, elmdata.data);
  });

  gingko.ports.dragstart.subscribe(function(event) {
    event.dataTransfer.setData("text", "");
    toElm("DragStarted", event.target.id.replace(/^card-/,""));
  });

  window.checkboxClicked = (cardId, number) => {
    toElm("CheckboxClicked", [cardId, number]);
  };

  // Session messages
  gingko.ports.storeSession.subscribe((data) => {
    if (data == null) {
      localStorage.removeItem(sessionStorageKey);
    } else {
      localStorage.setItem(sessionStorageKey, data);
      setUserDbs(data);
      setTimeout(()=> gingko.ports.sessionChanged.send(data), 0);
    }
  });

  // Whenever localStorage changes in another tab, report it if necessary.
  window.addEventListener("storage", function(event) {
      if (event.storageArea === localStorage && event.key === sessionStorageKey) {
        gingko.ports.sessionChanged.send(event.newValue);
      }
  }, false);
}


// Doc State Proxy Object

const docStateHandlers = {
  set: function(obj, prop, value) {
    if (typeof gingko !== "undefined") {
      switch(prop) {
        case "revs":
          toElm("SetRevs", value);
          break;

        case "lastSavedRemotely":
          toElm("SavedRemotely", value);
          break;

        case "lastSavedLocally":
          toElm("SavedLocally", value);
          break;

        case "changed":
          container.sendTo("doc:set-changed", value);
          break;
      }
    }
    obj[prop] = value;
    return true;
  }
};
const docState = new Proxy(container.getInitialDocState(), docStateHandlers);

container.msgWas("set-doc-state", (e, data) => {
  Object.assign(docState, data);
});

// ============ SYNC ====================

function setUserDbs(email) {
  console.log("Inside setUserDbs", email, helpers.toHex(email));
  const userDbName = `userdb-${helpers.toHex(email)}`;
  let userDbUrl = config.COUCHDB_SERVER + "/" + userDbName;
  userStore.db(email, userDbUrl);
  var remoteOpts =
    { skip_setup: true
    , fetch(url, opts){
        return fetch(url, opts);
      }
    };
  remoteDB = new PouchDB(userDbUrl, remoteOpts);
  self.db = new PouchDB(userDbName)
};

// ============ END SYNC ====================

function toElm (tag, data) {
  gingko.ports.infoForElm.send({tag: tag, data: data});
}




/* === Elm to JS Ports === */

const update = (msg, data) => {
  let cases =
    {
      // === Dialogs, Menus, Window State ===

      "Alert": () => { alert(data); }

    , "SetChanged" : () => {
        docState.changed = data;
      }

    , "ConfirmCancelCard": () => {
        let tarea = document.getElementById("card-edit-"+data[0]);

        if (tarea === null) {
          console.log("tarea not found");
        } else {
          if(tarea.value === data[1] || confirm(tr.areYouSureCancel[lang])) {
            docState.changed = false;
            toElm("CancelCardConfirmed", null);
          }
        }
      }

    , "ColumnNumberChange": () => {
        container.sendTo("doc:column-number-change", data);
      }

      // === Database ===
    , "InitDocument": async () => {
        TREE_ID = data;
        localStore.db(data);
        initMetadata(data);
    }

    , "LoadDocument": async () => {
        TREE_ID = data;
        localStore.db(data);
        loadMetadata();
        pull(true); // load local
        pull(false); // sync remote
      }

    , "RequestDelete": async () => {
      if(confirm("Are you sure you want to delete this document?")) {
        let docsFetch = await remoteDBnoTransform.allDocs({startkey: data+"/", endkey: data+"/\ufff0"});
        let docsToDelete = docsFetch.rows.map(r => {return {_id : r.id, _rev: r.value.rev, _deleted: true}});
        let deleteResponse = await remoteDBnoTransform.bulkDocs(docsToDelete);
        let localDBToDelete = new PouchDB(data);
        await localDBToDelete.destroy();
        toElm("DocListChanged", null);
      }
    }

    , "NoDataToSave": () => {
        if (actionOnData === ActionOnData.Exit) {
          // Empty document
          // Should close without saving.
          container.sendTo("doc:save-and-exit", true);
        }
      }

    , "SaveData": async () => {
        // Store ids of refs, so we can send back updated _rev.
        const refIds = data.refs.map(r => r._id);

        // Keep objects that are not saved in database.
        const newCommits = data.commits.filter( o => !savedObjectIds.includes(o._id));
        const newTreeObjects = data.treeObjects.filter( o => !savedObjectIds.includes(o._id));

        // Remove conflicts if head is updated
        let savedHead = await db.get(prefix("heads/master"), {conflicts: true}).catch(async e => e);
        if (savedHead.hasOwnProperty("_conflicts")) {
          let revToDelete = savedHead._conflicts[0];
          let delRes = await db.remove(prefix("heads/master"), revToDelete);
        }

        // Save to database, and add successes to savedObjectIds.
        const rows = [...newCommits, ...newTreeObjects, ...data.refs];
        const toSave = rows.map(r =>{r._id = prefix(r._id); return r;} );
        const responses = await db.bulkDocs(toSave);
        const savedIds = responses.filter(r => r.ok).map(o => o.id);
        savedObjectIds = savedObjectIds.concat(savedIds);

        // Send updated _revs for successful ref saves.
        const savedRefs = responses
                            .map(r => {r.id = unprefix(r.id); return r })
                            .filter(r => refIds.includes(r.id) && r.ok);
        toElm("DataSaved", savedRefs);
      }

    , "SaveImportedData": async () => {
        let savePromises = data.map(doc => {
          let treeDb = new PouchDB(doc.id);
          let dataRows = [...doc.data.commits, ...doc.data.treeObjects, ...doc.data.refs, doc.metadata];
          let toSave = dataRows.map(r => {
            r._id = doc.id + "/" + r._id;
            return _.omit(r, "_rev");
          });
          return remoteDBnoTransform.bulkDocs(toSave);
        });
        let saveResults = await Promise.all(savePromises);
        toElm("ImportComplete", null);
    }

    , "Push": push

    , "Pull": () => pull(false)

      // === File System ===

    , "ExportDOCX": () => {
        try {
          container.exportDocx(data.data, data.filepath);
        } catch (e) {
          container.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportJSON": () => {
        try {
          container.exportJson(data.data, data.filepath);
        } catch (e) {
          container.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportTXT": () => {
        try {
          container.exportTxt(data.data, data.filepath);
        } catch (e) {
          container.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportTXTColumn": () => {
        try {
          container.exportTxt(data.data, data.filepath);
        } catch (e) {
          container.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

      // === DOM ===

    , "ActivateCards": () => {
        lastActivesScrolled = data.lastActives;
        lastColumnScrolled = data.column;

        helpers.scrollHorizontal(data.column);
        helpers.scrollColumns(data.lastActives);

        localStore.set("last-active", data.cardId);
      }

    , "FlashCurrentSubtree": () => {
        let addFlashClass = function() {
          jQuery(".card.active").addClass("flash");
          jQuery(".group.active-descendant").addClass("flash");
        };

        let removeFlashClass = function() {
          jQuery(".card.active").removeClass("flash");
          jQuery(".group.active-descendant").removeClass("flash");
        };

        addFlashClass();
        setTimeout(removeFlashClass, 200);
      }

    , "TextSurround": () => {
        let id = data[0];
        let surroundString = data[1];
        let tarea = document.getElementById("card-edit-"+id);

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
      }

    , "SetCursorPosition": () => {
        let pos = data[0];
        setTimeout(() => document.activeElement.setSelectionRange(pos,pos), 0);
    }

      // === UI ===
    , "SaveMetadata": async () => {
        data._id = prefix(data._id);
        let saveRes = await db.put(data).catch(async e => e);
        if (saveRes.ok) {
          await db.replicate.to(remoteDB, {doc_ids: [prefix("metadata")]});
          data._rev = saveRes.rev;
          data._id = unprefix(data._id);
          toElm("MetadataSaved", data);
        } else {
          console.error(data, saveRes);
          toElm("MetadataSaveError", null);
        }
    }

    , "UpdateCommits": () => {
      }
    , "SetVideoModal": () => {
        userStore.set("video-modal-is-open", data);
      }

    , "SetFonts": () => { setFonts(docState.dbPath[1], data);}

    , "SetShortcutTray": () => {
        userStore.set("shortcut-tray-is-open", data);
      }

      // === Misc ===

    , "SocketSend": () => {
        collab = data;
      //socket.emit("collab", data)
      }

    , "ConsoleLogRequested": () =>
        console.error(data)

    };

  try {
    cases[msg]();
  } catch(err) {
    console.error("Unexpected message from Elm", err, msg, data);
  }
};






/* === JS to Elm Ports === */

function intentExportToElm ( format, selection, filepath) {
  _lastFormat = format;
  _lastSelection = selection;
  toElm("IntentExport", { format: format, selection : selection, filepath: filepath} );
}


container.msgWas("menu:close-document", () => { actionOnData = ActionOnData.Exit; toElm("GetDataToSave", null); });
container.msgWas("menu:save", () => { actionOnData = ActionOnData.Save; toElm("GetDataToSave", null ); });
container.msgWas("menu:save-as", () => { actionOnData = ActionOnData.SaveAs; toElm("GetDataToSave", null ); });
container.msgWas("menu:export-docx", () => intentExportToElm("docx", "all", null));
container.msgWas("menu:export-docx-current", () => intentExportToElm("docx", "current", null));
container.msgWas("menu:export-docx-column", (e, msg) => intentExportToElm("docx", {column: msg}, null));
container.msgWas("menu:export-txt", () => intentExportToElm("txt", "all", null));
container.msgWas("menu:export-txt-current", () => intentExportToElm("txt", "current", null));
container.msgWas("menu:export-txt-column", (e, msg) => intentExportToElm("txt", {column: msg}, null));
container.msgWas("menu:export-json", () => intentExportToElm("json", "all", null));
container.msgWas("menu:export-repeat", (e, lastExportPath) => intentExportToElm(_lastFormat, _lastSelection, lastExportPath));
container.msgWas("menu:undo", () => toElm("Keyboard", "mod+z"));
container.msgWas("menu:redo", () => toElm("Keyboard", "mod+shift+z"));
container.msgWas("menu:cut", () => toElm("Keyboard", "mod+x"));
container.msgWas("menu:copy", () => toElm("Keyboard", "mod+c"));
container.msgWas("menu:paste", () => toElm("Keyboard", "mod+v"));
container.msgWas("menu:paste-into", () => toElm("Keyboard", "mod+shift+v"));
container.msgWas("menu:view-videos", () => toElm("ViewVideos", null ));
container.msgWas("menu:font-selector", (event, data) => toElm("FontSelectorOpen", data));
container.msgWas("menu:language-select", (event, data) => {
  lang = data;
  userStore.set("language", data);
  container.sendTo("doc:language-changed", data);
  toElm("LanguageChanged", data);
});
container.msgWas("menu:toggle-support", (event, makeVisible) => {
  try {
    if (makeVisible) {
      helpWidgetLauncher.style.visibility = "visible";
      FreshworksWidget("open");
    } else {
      helpWidgetLauncher.style.visibility = "hidden";
      FreshworksWidget("close");
    }
    container.sendTo("doc:support-toggled", makeVisible);
  } catch (err) {
    let options =
      { title: "Failed to Open Online Help"
      , message: "Couldn't reach the online help desk.\nEither you are offline, or there's a bug. You can reach me at adriano@gingkoapp.com"
      , type: "info"
      };
    container.showMessageBox(options);
  }
});

//socket.on("collab", data => toElm("RecvCollabState", data))
//socket.on("collab-leave", data => toElm("CollaboratorDisconnected", data))






/* === Database === */


// Load device-specific data (unsynced settings)
async function loadLocalStore() {
  let store = await localStore.load();
  toElm("LocalStoreLoaded", store);
}


// Load user-specific data (synced settings)
async function loadUserStore() {
  let store = await userStore.load();
  lang = store.language;
  toElm("UserStoreLoaded", store);
}


async function initMetadata(docId) {
  const now = Date.now();
  let metadata = {_id : docId+"/metadata", docId: docId, name: null, createdAt: now, updatedAt: now};
  await db.put(metadata).catch(async e => e);
}

async function loadMetadata() {
  let metadata = await db.get("metadata").catch(async e => e);
  if (!metadata.error) {
    toElm("MetadataSaved", metadata);
  }
}


async function pull(isNew) {
  // Get local head before replication.
  let localHead = await db.get(prefix("heads/master")).catch(async (e) => e);

  // Fetch remote changes for current document.
  if (!isNew) {
    let selector = { "_id": { "$regex": `${TREE_ID}/` } };
    await db.replicate.from(remoteDB, {selector}).catch(async (e) => e);
  }

  // Get all local entries in db, and format in object to send to Elm.
  let result = await db.allDocs({include_docs: true, startkey: TREE_ID+"/", endkey: TREE_ID+"/\ufff0"})
  let mappedDocs = result.rows.map(r => {r.doc._id = unprefix(r.doc._id); return r.doc;});
  let groupFn = r => r.hasOwnProperty("type") ? r.type : r._id;
  let toSend = _.groupBy(mappedDocs, groupFn);

  // Get new head, or possible conflict/branching history.
  let newHead = await db.get(prefix("heads/master"), {conflicts: true}).catch(async (e) => e);

  // Add conflicts to data to be sent.
  if (newHead.hasOwnProperty("_conflicts")) {
    let conflictHead = await db.get(prefix("heads/master"), {rev: newHead._conflicts[0]});
    if (_.isEqual(localHead, conflictHead)) {
      let setHead = (r) => { return (r._id == prefix("heads/master") ? conflictHead : r); }
      toSend.ref = toSend.ref.map(setHead);
      toSend.conflict = newHead;
    } else {
      toSend.conflict = conflictHead;
    }
  }

  // Finally, send all objects into Elm repo.
  toSend.isNew = isNew;
  toElm("DataReceived",toSend);
  if (toSend.metadata) { toElm("MetadataSynced", toSend.metadata[0]) }
}


async function push() {
  let selector = { "_id": { "$regex": `${TREE_ID}/` } };
  await db.replicate.to(remoteDB, {selector}).catch(async (e) => e);
}


/* === Local Functions === */


function prefix(id) {
  return TREE_ID+"/"+id;
}

function unprefix(id) {
  return id.slice(TREE_ID.length + 1);
}


const saveErrorAlert = (err) => {
  return errorAlert(tr.saveError[lang],tr.saveErrorMsg[lang], err);
};


function setLastActive (filepath, lastActiveCard) {
  if (typeof filepath === "string") {
    userStore.set(`last-active-cards.${filepath.replace(".","\\.")}`, lastActiveCard);
  }
}


function getLastActive (filepath) {
  if (typeof filepath === "string") {
    let lastActiveCard = userStore.get(`last-active-cards.${filepath.replace(".","\\.")}`);
    if (typeof lastActiveCard === "string") {
      return lastActiveCard;
    } else {
      return "1";
    }
  } else {
    return "1";
  }
}


function setFonts (filepath, fonts) {
  if (typeof filepath === "string") {
    userStore.set(`fonts.${filepath.replace(".","\\.")}`, fonts);
  }
}


function getFonts (filepath) {
  if (typeof filepath === "string") {
    let fonts = userStore.get(`fonts.${filepath.replace(".","\\.")}`);
    if (Array.isArray(fonts) && fonts.length == 3) {
      return fonts;
    } else {
      return null;
    }
  } else {
    return null;
  }
}




/* === DOM Events and Handlers === */

// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
  ev.preventDefault();
};

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


const editingInputHandler = function(ev) {
  if(docState.changed !== true) {
    docState.changed = true;
  }
  toElm("FieldChanged", ev.target.value);
  selectionHandler(ev);
  //collab.field = ev.target.value
  //socket.emit('collab', collab)
};

const selectionHandler = function(ev) {
  if(document.activeElement.nodeName == "TEXTAREA") {
    let {selectionStart, selectionEnd, selectionDirection} = document.activeElement;
    let length = document.activeElement.value.length;
    let [before,after] = [document.activeElement.value.substring(0,selectionStart), document.activeElement.value.substring(selectionStart) ]
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

    toElm("TextCursor",
      { selected: selectionStart !== selectionEnd
      , position: cursorPosition
      , text: [before, after]
      }
    );
  }
};

document.onselectionchange = selectionHandler;


Mousetrap.bind(helpers.shortcuts, function(e, s) {
  toElm("Keyboard",s);

  if(helpers.needOverride.includes(s)) {
    return false;
  }
});


Mousetrap.bind(["tab"], function(e, s) {
  document.execCommand("insertText", false, "  ");
  return false;
});

Mousetrap.bind(["shift+tab"], function(e, s) {
  return true;
});


/* === DOM manipulation === */


const observer = new MutationObserver(function(mutations) {
  const isTextarea = function(node) {
    return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap";
  };

  const isHelpWidget = function(node) {
    return node.nodeName == "IFRAME" && node.id == "launcher-frame";
  };

  let textareas = [];

  mutations
    .map( m => {
          [].slice.call(m.addedNodes)
            .map(n => {
              if (isTextarea(n)) {
                textareas.push(n);
              } else if (isHelpWidget(n)) {
                helpWidgetLauncher = n;
                if (!helpVisible) {
                  helpWidgetLauncher.style.visibility = "hidden";
                }
              } else {
                if(n.querySelectorAll) {
                  let tareas = [].slice.call(n.querySelectorAll("textarea.edit"));
                  textareas = textareas.concat(tareas);
                }
              }
            });
        });

  if (textareas.length !== 0) {
    textareas.map(t => {
      t.oninput = editingInputHandler;
    });

    container.sendTo("doc:edit-mode-toggle", true);
    jQuery(textareas).textareaAutoSize();
  } else {
    container.sendTo("doc:edit-mode-toggle", false);
  }
});

const observerConfig = { childList: true, subtree: true };

observer.observe(document.body, observerConfig);
