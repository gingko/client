const jQuery = require("jquery");
const _ = require("lodash");
require("textarea-autosize");
const Mousetrap = require("mousetrap");
const container = require("Container");

import PouchDB from "pouchdb";
//PouchDB.plugin(require("/home/adriano/code/oss/transform-pouch"));
PouchDB.plugin(require("pouchdb-migrate"));


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
let helpWidgetLauncher;

const ActionOnData =
  { Exit: "Exit"
  , Save: "Save"
  , SaveAs: "SaveAs"
  };
let actionOnData = ActionOnData.Save;


/* === Initializing App === */

const userStore = container.userStore;
var lang = userStore.get("language") || "en";
var helpVisible = userStore.get("help-visible") || true;
self.savedObjectIds = [];


const docStateHandlers = {
  set: function(obj, prop, value) {
    if (typeof gingko !== "undefined") {
      switch(prop) {
        case "headRev":
          toElm("SetHeadRev", value);
          break;

        case "dbPath":
          console.log(`new PouchDB: ${value[0]}`);
          self.db = new PouchDB(value[0]);
          break;

        case "lastSavedToFile":
          toElm("SetLastFileSaved", value);
          break;

        case "lastSavedToDB":
          toElm("SetLastCommitSaved", value);
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

container.answerMain("set-doc-state", data => {
  Object.assign(docState, data);
});

self.db = new PouchDB(docState.dbPath[0]);
console.log(`new PouchDB: ${docState.dbPath[0]}`);

self.enableSync = async (treeId) => {
  self.TREE_ID = treeId;
  var head = await self.db.get("heads/master");

  const currRows = await self.db.allDocs({include_docs: true});
  const newDocs = currRows.rows.map(r => { r.doc.treeId = treeId; return r.doc });
  console.log("newDocs", newDocs);
  const bulkRes = await self.db.bulkDocs(newDocs);

  head = await self.db.get("heads/master");
  console.log("head after", head, bulkRes);
  toElm("SetHeadRev", head._rev);
};

self.remoteDB = new PouchDB("http://localhost:5984/test-filtered-2");
//self.remoteDB.transform(
//  { outgoing: (doc) => {
//      doc._id = doc._id.slice(5);
//      return doc;
//    }
//  , incoming: (doc) => {
//      doc._id = "4567/" + doc._id;
//      return doc;
//    }
//});

container.msgWas("main:database-close", async () => {
  await db.close();
});
container.msgWas("main:database-open", async () => {
  self.db = new PouchDB(docState.dbPath[0]);
});

if (docState.jsonImportData) {
  const initFlags =
    [ docState.jsonImportData
      , { language : lang
        , isMac : process.platform === "darwin"
        , shortcutTrayOpen : userStore.get("shortcut-tray-is-open", true)
        , videoModalOpen : userStore.get("video-modal-is-open", false)
        , currentTime : Date.now()
        , lastCommitSaved : null
        , lastFileSaved : null
        , lastActive : getLastActive(docState.dbPath[1])
        , fonts : getFonts(docState.dbPath[1])
        }
      , false // isSaved
    ];

  initElmAndPorts(initFlags);
} else {
  load().then(function (dbData) {

    savedObjectIds = Object.keys(dbData[1].commits).concat(Object.keys(dbData[1].treeObjects));

    docState.lastSavedToDB = Object.values(dbData[1].commits).map(c => c.timestamp).sort().slice(-1)[0];

    const initFlags =
      [ dbData
        , { language : lang
          , isMac : process.platform === "darwin"
          , shortcutTrayOpen : userStore.get("shortcut-tray-is-open", true)
          , videoModalOpen : userStore.get("video-modal-is-open", false)
          , currentTime : Date.now()
          , lastCommitSaved : docState.lastSavedToDB || null
          , lastFileSaved : docState.lastSavedToFile || null
          , lastActive : getLastActive(docState.dbPath[1])
          , fonts : getFonts(docState.dbPath[1])
          }
        , !!docState.dbPath[1] // isSaved
      ];

    initElmAndPorts(initFlags);
  });
}


//self.socket = io.connect('http://localhost:3000')


function initElmAndPorts(initFlags) {
  self.gingko = Elm.Main.init({ node: document.getElementById("elm"), flags: initFlags});

  gingko.ports.infoForOutside.subscribe(function(elmdata) {
    update(elmdata.tag, elmdata.data);
  });

  gingko.ports.dragstart.subscribe(function(event) {
    event.dataTransfer.setData("text", "");
    toElm("DragStarted", event.target.id.replace(/^card-/,""));
  });

  window.onbeforeunload = (e) => {
    actionOnData = ActionOnData.Exit;
    toElm("GetDataToSave", null);
    e.returnValue = false;
  };

  window.checkboxClicked = (cardId, number) => {
    toElm("CheckboxClicked", [cardId, number]);
  };
}


function toElm (tag, data) {
  self.gingko.ports.infoForElm.send({tag: tag, data: data});
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

    , "CommitWithTimestamp": () => {
        toElm("Commit", Date.now());
      }

    , "SaveToDB": async () => {
        try {
          const { headRev, lastSavedToDB } = await saveToDB(data[0], data[1]);
          docState.headRev = headRev;
          docState.lastSavedToDB = lastSavedToDB;

          switch(actionOnData) {
            case ActionOnData.Save:
              container.sendTo("doc:save");
              break;

            case ActionOnData.SaveAs:
              container.sendTo("doc:save-as");
              actionOnData = ActionOnData.Save;
              break;

            case ActionOnData.Exit:
              if (data[1].commits.length == 1 && data[1].commits[0].tree == "38b64ce2726abefc56db43a526ba88269c946751") {
                // Empty document with blank initial commit
                // Should close without saving.
                container.sendTo("doc:save-and-exit", true);
              } else {
                container.sendTo("doc:save-and-exit", false);
              }
              actionOnData = ActionOnData.Save;
              break;
          }
        } catch (e) {
          container.showMessageBox(saveErrorAlert(e));
          return;
        }
      }

    , "Push": push

    , "Pull": sync

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

        setLastActive(docState.dbPath[1], data.cardId);
        helpers.scrollHorizontal(data.column);
        helpers.scrollColumns(data.lastActives);
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

      // === UI ===

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
        console.log(data)

    };

  try {
    cases[msg]();
  } catch(err) {
    console.log("elmCases one-port failed:", err, msg, data);
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
  toElm("SetLanguage", data);
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

function processData (data, type) {
  var processed = data.filter(d => d.type === type).map(d => _.omit(d, "type"));
  var dict = {};
  if (type == "ref") {
    processed.map(d => dict[d._id] = _.omit(d, "_id"));
  } else {
    processed.map(d => dict[d._id] = _.omit(d, ["_id","_rev"]));
  }
  return dict;
}


function load(filepath, headOverride){
  return new Promise( (resolve, reject) => {
    db.info().then(function (result) {
      if (result.doc_count == 0) {
        let toSend = [{_id: "status" , status : "bare", bare: true}, { commits: {}, treeObjects: {}, refs: {}}];
        resolve(toSend);
      } else {

        db.get("status")
          .catch(err => {
            if(err.name == "not_found") {
              console.log("load status not found. Setting to \"bare\".");
              return {_id: "status" , status : "bare", bare: true};
            } else {
              reject("load status error" + err);
            }
          })
          .then(statusDoc => {
            status = statusDoc.status;

            db.allDocs(
              { include_docs: true
              }).then(function (result) {
              let data = result.rows.map(r => r.doc);

              let commits = processData(data, "commit");
              let trees = processData(data, "tree");
              let refs = processData(data, "ref");
              let status = _.omit(statusDoc, "_rev");

              if(headOverride) {
                refs["heads/master"] = headOverride;
              } else if (_.isEmpty(refs)) {
                var keysSorted = Object.keys(commits).sort(function(a,b) { return commits[b].timestamp - commits[a].timestamp; });
                var lastCommit = keysSorted[0];
                if (lastCommit) {
                  refs["heads/master"] = { value: lastCommit, ancestors: [], _rev: "" };
                  console.log("recovered status", status);
                  console.log("refs recovered", refs);
                }
              }

              let toSend = [status, { commits: commits, treeObjects: trees, refs: refs}];
              resolve(toSend);
            }).catch(function (err) {
              container.showMessageBox(errorAlert(tr.loadingError[lang], tr.loadingErrorMsg[lang], err));
              reject(err);
            });
        });
      }
    });
  });
}

const merge = function(local, remote){
  db.allDocs( { include_docs: true })
    .then(function (result) {
      data = result.rows.map(r => r.doc);

      let commits = processData(data, "commit");
      let trees = processData(data, "tree");
      let refs = processData(data, "ref");

      let toSend = { commits: commits, treeObjects: trees, refs: refs};
      toElm("Merge", [local, remote, toSend]);
    }).catch(function (err) {
      console.log(err);
    });
};


function pull (local, remote, info) {
  db.replicate.from(remoteCouch)
    .on("complete", pullInfo => {
      if(pullInfo.docs_written > 0 && pullInfo.ok) {
        merge(local, remote);
      }
    });
}


function push () {
  db.replicate.to(remoteCouch);
}


function sync () {
  db.get("heads/master")
    .then(localHead => {
      remoteDb.get("heads/master")
        .then(remoteHead => {
          if(_.isEqual(localHead, remoteHead)) {
            // Local == Remote => no changes
            console.log("up-to-date");
          } else if (localHead.ancestors.includes(remoteHead.value)) {
            // Local is ahead of remote => Push
            push("push:Local ahead of remote");
          } else {
            // Local is behind of remote => Pull
            pull(localHead.value, remoteHead.value, "Local behind remote => Fetch & Merge");
          }
        })
        .catch(remoteHeadErr => {
          if(remoteHeadErr.name == "not_found") {
            // Bare remote repository => Push
            push("push:bare-remote");
          }
        });
    })
    .catch(localHeadErr => {
      remoteDb.get("heads/master")
        .then(remoteHead => {
          if(localHeadErr.name == "not_found") {
            // Bare local repository => Pull
            pull(null, remoteHead.value, "Bare local => Fetch & Merge");
          }
        })
        .catch(remoteHeadErr => {
          if(remoteHeadErr.name == "not_found") {
            // Bare local & remote => up-to-date
            push("up-to-date (bare)");
          }
        });
    });
}



/* === Local Functions === */

self.saveToDB = (status, objects) => {
  if(typeof self.TREE_ID === "string" && self.TREE_ID.length > 0) {
    console.log("self.TREE_ID:", self.TREE_ID);
    console.log(objects, status);
    status.treeId = self.TREE_ID;
    objects.commits.map( c => c.treeId = self.TREE_ID );
    objects.treeObjects.map( t => t.treeId = self.TREE_ID );
    objects.refs.map( r => r.treeId = self.TREE_ID );
  }

  return new Promise(
    async (resolve, reject) => {
      try {
        var statusDoc =
          await db.get("status")
                .catch(err => {
                  if(err.name == "not_found") {
                    return {_id: "status" , status : "bare", bare: true};
                  } else {
                    console.log("load status error", err);
                  }
                });
      } catch (e) {
        reject(e);
        return;
      }

      if(statusDoc._rev) {
        status["_rev"] = statusDoc._rev;
      }

      const lastSavedToDB = Object.values(objects.commits).map(c => c.timestamp).sort().slice(-1)[0];

      // Filter out object that are already saved in database
      const newCommits = objects.commits.filter( o => !savedObjectIds.includes(o._id));
      const newTreeObjects = objects.treeObjects.filter( o => !savedObjectIds.includes(o._id));

      const toSave = [...newCommits, ...newTreeObjects, ...objects.refs, ...[status]];

      try {
        var responses = await db.bulkDocs(toSave);
        let savedIds = responses.filter(r => r.ok && r.id !== "status" && r.id !== "heads/master");
        savedObjectIds = savedObjectIds.concat(savedIds.map( o => o.id));
      } catch (e) {
        reject(e);
        return;
      }

      let head = responses.filter(r => r.id == "heads/master")[0];
      if (head.ok) {
        resolve({ headRev: head.rev, lastSavedToDB });
      } else {
        reject(new Error(`Reference error when saving to DB.\n${head}`));
        return;
      }
    });
};


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


document.addEventListener("click", (ev) => {
  if(ev.target.nodeName == "A") {
    ev.preventDefault();
    container.openExternal(ev.target.href);
  }
});


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

const config = { childList: true, subtree: true };

observer.observe(document.body, config);
