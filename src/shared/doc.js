const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')
const Mousetrap = require('mousetrap')
const server = require("Server");

const {ipcRenderer, remote, webFrame } = require('electron')
const {app} = remote
const querystring = require('querystring')
const Store = require('electron-store')

import PouchDB from "pouchdb";


//const io = require('socket.io-client')

const helpers = require("./doc-helpers");
const errorAlert = helpers.errorAlert;
const { tr } = require("../shared/translation.js");
import { Elm } from "../elm/Main";



/* === Global Variables === */

const userStore = new Store({name: "config"})
var lastActivesScrolled = null
var lastColumnScrolled = null
var _lastFormat = null
var _lastSelection = null
var collab = {}
self.savedObjectIds = [];

const ActionOnData = Object.freeze(
  { Exit : Symbol("Exit")
  , Save : Symbol("Save")
  , SaveAs : Symbol("SaveAs")
  }
);
var actionOnData = ActionOnData.Save;

const SaveState = Object.freeze(
  { Changed: {Saved : Symbol("Changed.Saved"), SavedDB: Symbol("Changed.SavedDB")}
  , SavedDB: Symbol("SavedDB")
  , Saved: Symbol("Saved")
  }
);
var saveState = SaveState.SavedDB;


/* === Initializing App === */

console.log('Gingko version', app.getVersion())


var firstRun = userStore.get('first-run', true)
var lang = userStore.get("language") || "en";
var docWindow = remote.getCurrentWindow()
var jsonImportData = docWindow.jsonImportData;
var currentPath = docWindow.originalPath;

self.db = new PouchDB(docWindow.dbPath);
ipcRenderer.on("database-close", async () => {
  await db.close();
});
ipcRenderer.on("database-open", async () => {
  self.db = new PouchDB(docWindow.dbPath);
});

if(!!jsonImportData) {
  saveState = SaveState.SavedDB;
  var initFlags =
    [ jsonImportData
      , { language : lang
        , isMac : process.platform === "darwin"
        , shortcutTrayOpen : userStore.get('shortcut-tray-is-open', true)
        , videoModalOpen : userStore.get('video-modal-is-open', false)
        , currentTime : Date.now()
        , lastActive : getLastActive(currentPath)
        , fonts : getFonts(currentPath)
        }
      , false // isSaved
    ]

  initElmAndPorts(initFlags);
} else {
  load().then(function (dbData) {

    savedObjectIds = Object.keys(dbData[1].commits).concat(Object.keys(dbData[1].treeObjects))

    saveState = currentPath ? SaveState.Saved : SaveState.SavedDB;

    var initFlags =
      [ dbData
        , { language : lang
          , isMac : process.platform === "darwin"
          , shortcutTrayOpen : userStore.get('shortcut-tray-is-open', true)
          , videoModalOpen : userStore.get('video-modal-is-open', false)
          , currentTime : Date.now()
          , lastActive : getLastActive(currentPath)
          , fonts : getFonts(currentPath)
          }
        , !!currentPath // isSaved
      ]

    initElmAndPorts(initFlags);
  })
}


//self.socket = io.connect('http://localhost:3000')


function initElmAndPorts(initFlags) {
  self.gingko = Elm.Main.init({ node: document.getElementById("elm"), flags: initFlags});

  gingko.ports.infoForOutside.subscribe(function(elmdata) {
    update(elmdata.tag, elmdata.data)
  })

  gingko.ports.dragstart.subscribe(function(event) {
    event.dataTransfer.setData("text", "");
    toElm("DragStarted", event.target.id.replace(/^card-/,""));
  });

  window.onbeforeunload = (e) => {
    actionOnData = ActionOnData.Exit;
    toElm("GetDataToSave", null)
    e.returnValue = false
  }
}


function toElm (tag, data) {
  self.gingko.ports.infoForElm.send({tag: tag, data: data})
}




//self.remoteCouch = 'http://localhost:5984/atreenodes16'
//self.remoteDb = new PouchDB(remoteCouch)

var crisp_loaded = false;

// Needed for unit tests
window.$crisp = (typeof $crisp === 'undefined') ? [] : $crisp

$crisp.push(['do', 'chat:hide'])
$crisp.push(['on', 'session:loaded', () => { crisp_loaded = true }])
$crisp.push(['on', 'chat:closed', () => { $crisp.push(['do', 'chat:hide']) }])
$crisp.push(['on', 'chat:opened', () => { $crisp.push(['do', 'chat:show']) }])
$crisp.push(['on', 'message:received', () => { $crisp.push(['do', 'chat:show']) }])
if (firstRun) {
  var ctrlOrCmd = process.platform === "darwin" ? "⌘" : "Ctrl";
  userStore.set('first-run', false)
  $crisp.push(['do'
              , 'message:show'
              , [ 'text' ,
`Hi! Try these steps to get started:
1. **Enter** to start writing
2. **${ctrlOrCmd} + Enter** to save changes
3. **${ctrlOrCmd} + →** to write in a new *child* card
4. **${ctrlOrCmd} + Enter** to save changes
5. **${ctrlOrCmd} + ↓**

I know it's not much guidance, but it's a start.
**Help > Contact Adriano** to send me a message.

---
*PS: I won't interrupt again, except to respond.*
*Your attention is sacred.*`
                ]
              ]
             )
}


/* === Elm to JS Ports === */

const update = (msg, data) => {
  let cases =
    {
      // === Dialogs, Menus, Window State ===

      "Alert": () => { alert(data) }

    , "SetChanged" : () => {
        ipcRenderer.send("doc:set-changed", data);
        if (saveState == SaveState.SavedDB) {
          saveState = SaveState.Changed.SavedDB;
        } else if (saveState == SaveState.Saved) {
          saveState = SaveState.Changed.Saved;
        }
      }

    , "ConfirmCancelCard": () => {
        let tarea = document.getElementById("card-edit-"+data[0])

        if (tarea === null) {
          console.log("tarea not found")
        } else {
          if(tarea.value === data[1] || confirm(tr.areYouSureCancel[lang])) {
            ipcRenderer.send("doc:set-changed", false);
            if(saveState == SaveState.Changed.SavedDB){
              saveState = SaveState.SavedDB;
              toElm("SetSaveStatus", "SavedDB");
            } else if(saveState == SaveState.Changed.Saved){
              saveState = SaveState.Saved;
              toElm("SetSaveStatus", "Saved");
            }
            toElm("CancelCardConfirmed", null);
          }
        }
      }

    , "ColumnNumberChange": () => {
        ipcRenderer.send("doc:column-number-change", data)
      }

      // === Database ===

    , "CommitWithTimestamp": () => {
        toElm("Commit", Date.now());
      }

    , "SaveToDB": async () => {
        try {
          var newHeadRev = await saveToDB(data[0], data[1])
          saveState = SaveState.SavedDB;
          toElm("SetHeadRev", newHeadRev)

          switch(actionOnData) {
            case ActionOnData.Save:
              ipcRenderer.send("doc:save");
              break;

            case ActionOnData.SaveAs:
              ipcRenderer.send("doc:save-as");
              actionOnData = ActionOnData.Save;
              break;

            case ActionOnData.Exit:
              ipcRenderer.send("doc:save-and-exit");
              actionOnData = ActionOnData.Save;
              break;
          }
        } catch (e) {
          server.showMessageBox(saveErrorAlert(e))
          return;
        }
      }

    , "Push": push

    , "Pull": sync

      // === File System ===

    , "ExportDOCX": () => {
        try {
          server.exportDocx(data.data, data.filepath);
        } catch (e) {
          server.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportJSON": () => {
        try {
          server.exportJson(data.data, data.filepath)
        } catch (e) {
          server.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportTXT": () => {
        try {
          server.exportTxt(data.data, data.filepath)
        } catch (e) {
          server.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

    , "ExportTXTColumn": () => {
        try {
          server.exportTxt(data.data, data.filepath)
        } catch (e) {
          server.showMessageBox(errorAlert(tr.exportError[lang], tr.exportErrorMsg[lang], e));
          return;
        }
      }

      // === DOM ===

    , "ActivateCards": () => {
        lastActivesScrolled = data.lastActives;
        lastColumnScrolled = data.column;

        setLastActive(currentPath, data.cardId);
        helpers.scrollHorizontal(data.column);
        helpers.scrollColumns(data.lastActives);
      }

    , "FlashCurrentSubtree": () => {
        let addFlashClass = function() {
          jQuery(".card.active").addClass("flash")
          jQuery(".group.active-descendant").addClass("flash")
        }

        let removeFlashClass = function() {
          jQuery(".card.active").removeClass("flash")
          jQuery(".group.active-descendant").removeClass("flash")
        }

        addFlashClass()
        setTimeout(removeFlashClass, 200)
      }

    , "TextSurround": () => {
        let id = data[0]
        let surroundString = data[1]
        let tarea = document.getElementById("card-edit-"+id)

        if (tarea === null) {
          console.log("Textarea not found for TextSurround command.")
        } else {
          let start = tarea.selectionStart
          let end = tarea.selectionEnd
          if (start !== end) {
            let text = tarea.value.slice(start, end)
            let modifiedText = surroundString + text + surroundString
            document.execCommand("insertText", true, modifiedText)
          }
        }
      }

      // === UI ===

    , "UpdateCommits": () => {
        let commitGraphData = _.sortBy(data[0].commits, "timestamp").reverse().map(c => { return {sha: c._id, parents: c.parents}})
        let selectedSha = data[1]

        /*
        let commitElement = React.createElement(CommitsGraph, {
          commits: commitGraphData,
          onClick: setHead,
          selected: selectedSha
        });
        */

        //ReactDOM.render(commitElement, document.getElementById("history"))
    }
    , "SetVideoModal": () => {
        userStore.set("video-modal-is-open", data)
      }

    , "SetFonts": () => { setFonts(currentPath, data);}

    , "SetShortcutTray": () => {
        userStore.set("shortcut-tray-is-open", data)
      }

      // === Misc ===

    , "SocketSend": () => {
        collab = data
      //socket.emit("collab", data)
      }

    , "ConsoleLogRequested": () =>
        console.log(data)

    }

  try {
    cases[msg]();
  } catch(err) {
    console.log("elmCases one-port failed:", err, msg, data)
  }
}






/* === JS to Elm Ports === */

function intentExportToElm ( format, selection, filepath) {
  _lastFormat = format;
  _lastSelection = selection;
  toElm("IntentExport", { format: format, selection : selection, filepath: filepath} );
}

ipcRenderer.on("main:set-swap-folder", async (e, newPaths) => {
  self.db = new PouchDB(newPaths[0]);
  currentPath = newPaths[1];
});

ipcRenderer.on("main:saved-file", () => {
  toElm("SetSaveStatus", "Saved");
});


ipcRenderer.on("menu-close-document", () => { actionOnData = ActionOnData.Exit; toElm("GetDataToSave", null); });
ipcRenderer.on("menu-save", () => { actionOnData = ActionOnData.Save; toElm("GetDataToSave", null ); });
ipcRenderer.on("menu-save-as", () => { actionOnData = ActionOnData.SaveAs; toElm("GetDataToSave", null ); });
server.message.on("menu-export-docx", () => intentExportToElm("docx", "all", null));
ipcRenderer.on("menu-export-docx-current", () => intentExportToElm("docx", "current", null));
ipcRenderer.on("menu-export-docx-column", (e, msg) => intentExportToElm("docx", {column: msg}, null));
ipcRenderer.on("menu-export-txt", () => intentExportToElm("txt", "all", null));
ipcRenderer.on("menu-export-txt-current", () => intentExportToElm("txt", "current", null));
ipcRenderer.on("menu-export-txt-column", (e, msg) => intentExportToElm("txt", {column: msg}, null));
ipcRenderer.on("menu-export-json", () => intentExportToElm("json", "all", null));
ipcRenderer.on("menu-export-repeat", (e, lastExportPath) => intentExportToElm(_lastFormat, _lastSelection, lastExportPath));
ipcRenderer.on("menu-undo", () => toElm("Keyboard", "mod+z"));
ipcRenderer.on("menu-redo", () => toElm("Keyboard", "mod+shift+z"));
ipcRenderer.on("menu-cut", () => toElm("Keyboard", "mod+x"));
ipcRenderer.on("menu-copy", () => toElm("Keyboard", "mod+c"));
ipcRenderer.on("menu-paste", () => toElm("Keyboard", "mod+v"));
ipcRenderer.on("menu-paste-into", () => toElm("Keyboard", "mod+shift+v"));
ipcRenderer.on("menu-view-videos", () => toElm("ViewVideos", null ));
ipcRenderer.on("menu-font-selector", (event, data) => toElm("FontSelectorOpen", data));
ipcRenderer.on("menu-language-select", (event, data) => {
  lang = data;
  userStore.set("language", data);
  ipcRenderer.send("doc:language-changed", data);
  toElm("SetLanguage", data);
});
ipcRenderer.on("menu-contact-support", () => {
  if(crisp_loaded) {
    window.$crisp.push(["do", "chat:open"]);
    window.$crisp.push(["do", "chat:show"]);
  } else {
    server.openExternal("mailto:adriano@gingkoapp.com");
  }
});

//socket.on("collab", data => toElm("RecvCollabState", data))
//socket.on("collab-leave", data => toElm("CollaboratorDisconnected", data))






/* === Database === */

function processData (data, type) {
  var processed = data.filter(d => d.type === type).map(d => _.omit(d, 'type'))
  var dict = {}
  if (type == "ref") {
    processed.map(d => dict[d._id] = _.omit(d, '_id'))
  } else {
    processed.map(d => dict[d._id] = _.omit(d, ['_id','_rev']))
  }
  return dict
}


function load(filepath, headOverride){
  return new Promise( (resolve, reject) => {
    db.info().then(function (result) {
      if (result.doc_count == 0) {
        let toSend = [{_id: 'status' , status : 'bare', bare: true}, { commits: {}, treeObjects: {}, refs: {}}];
        resolve(toSend)
      } else {

        db.get('status')
          .catch(err => {
            if(err.name == "not_found") {
              console.log('load status not found. Setting to "bare".')
              return {_id: 'status' , status : 'bare', bare: true}
            } else {
              reject('load status error' + err)
            }
          })
          .then(statusDoc => {
            status = statusDoc.status;

            db.allDocs(
              { include_docs: true
              }).then(function (result) {
              let data = result.rows.map(r => r.doc)

              let commits = processData(data, "commit");
              let trees = processData(data, "tree");
              let refs = processData(data, "ref");
              let status = _.omit(statusDoc, '_rev')

              if(headOverride) {
                refs['heads/master'] = headOverride
              } else if (_.isEmpty(refs)) {
                var keysSorted = Object.keys(commits).sort(function(a,b) { return commits[b].timestamp - commits[a].timestamp })
                var lastCommit = keysSorted[0]
                if (!!lastCommit) {
                  refs['heads/master'] = { value: lastCommit, ancestors: [], _rev: "" }
                  console.log('recovered status', status)
                  console.log('refs recovered', refs)
                }
              }

              let toSend = [status, { commits: commits, treeObjects: trees, refs: refs}];
              resolve(toSend)
            }).catch(function (err) {
              server.showMessageBox(errorAlert(tr.loadingError[lang], tr.loadingErrorMsg[lang], err));
              reject(err)
            })
        })
      }
    })
  })
}

const merge = function(local, remote){
  db.allDocs( { include_docs: true })
    .then(function (result) {
      data = result.rows.map(r => r.doc)

      let commits = processData(data, "commit");
      let trees = processData(data, "tree");
      let refs = processData(data, "ref");

      let toSend = { commits: commits, treeObjects: trees, refs: refs};
      toElm('Merge', [local, remote, toSend]);
    }).catch(function (err) {
      console.log(err)
    })
}


function pull (local, remote, info) {
  db.replicate.from(remoteCouch)
    .on('complete', pullInfo => {
      if(pullInfo.docs_written > 0 && pullInfo.ok) {
        merge(local, remote)
      }
    })
}


function push () {
  db.replicate.to(remoteCouch)
}


function sync () {
  db.get('heads/master')
    .then(localHead => {
      remoteDb.get('heads/master')
        .then(remoteHead => {
          if(_.isEqual(localHead, remoteHead)) {
            // Local == Remote => no changes
            console.log('up-to-date')
          } else if (localHead.ancestors.includes(remoteHead.value)) {
            // Local is ahead of remote => Push
            push('push:Local ahead of remote')
          } else {
            // Local is behind of remote => Pull
            pull(localHead.value, remoteHead.value, 'Local behind remote => Fetch & Merge')
          }
        })
        .catch(remoteHeadErr => {
          if(remoteHeadErr.name == 'not_found') {
            // Bare remote repository => Push
            push('push:bare-remote')
          }
        })
    })
    .catch(localHeadErr => {
      remoteDb.get('heads/master')
        .then(remoteHead => {
          if(localHeadErr.name == 'not_found') {
            // Bare local repository => Pull
            pull(null, remoteHead.value, 'Bare local => Fetch & Merge')
          }
        })
        .catch(remoteHeadErr => {
          if(remoteHeadErr.name == 'not_found') {
            // Bare local & remote => up-to-date
            push('up-to-date (bare)')
          }
        })
    })
}



/* === Local Functions === */

self.saveToDB = (status, objects) => {
  return new Promise(
    async (resolve, reject) => {
      try {
        var statusDoc =
          await db.get('status')
                .catch(err => {
                  if(err.name == "not_found") {
                    return {_id: 'status' , status : 'bare', bare: true}
                  } else {
                    console.log('load status error', err)
                  }
                })
      } catch (e) {
        reject(e)
        return;
      }

      if(statusDoc._rev) {
        status['_rev'] = statusDoc._rev
      }


      // Filter out object that are already saved in database
      objects.commits = objects.commits.filter( o => !savedObjectIds.includes(o._id))
      objects.treeObjects = objects.treeObjects.filter( o => !savedObjectIds.includes(o._id))

      let toSave = objects.commits.concat(objects.treeObjects).concat(objects.refs).concat([status]);

      try {
        var responses = await db.bulkDocs(toSave)
        let savedIds = responses.filter(r => r.ok && r.id !== "status" && r.id !== "heads/master")
        savedObjectIds = savedObjectIds.concat(savedIds.map( o => o.id))
      } catch (e) {
        reject(e)
        return;
      }

      let head = responses.filter(r => r.id == "heads/master")[0]
      if (head.ok) {
        resolve(head.rev)
      } else {
        console.log(responses);
        reject(new Error(`Reference error when saving to DB.\n${head}`))
        return;
      }
    })
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
  ev.preventDefault()
}

window.onresize = () => {
  if (lastActivesScrolled) {
    debouncedScrollColumns(lastActivesScrolled)
  }
  if (lastColumnScrolled) {
    debouncedScrollHorizontal(lastColumnScrolled)
  }
}

const debouncedScrollColumns = _.debounce(helpers.scrollColumns, 200)
const debouncedScrollHorizontal = _.debounce(helpers.scrollHorizontal, 200)


const editingInputHandler = function(ev) {
  if (saveState == SaveState.Saved) {
    saveState = SaveState.Changed.Saved;
  } else if (saveState == SaveState.SavedDB) {
    saveState = SaveState.Changed.SavedDB;
  }
  toElm('FieldChanged', ev.target.value);
  ipcRenderer.send("doc:set-changed", true);
  selectionHandler(ev);
  //collab.field = ev.target.value
  //socket.emit('collab', collab)
}

const selectionHandler = function(ev) {
  if(document.activeElement.nodeName == "TEXTAREA") {
    let {selectionStart, selectionEnd, selectionDirection} = document.activeElement;
    let length = document.activeElement.value.length;
    let cursorPosition = "other";

    if (length == 0) {
      cursorPosition = "empty"
    } else if (selectionStart == 0 && selectionEnd == 0) {
      cursorPosition = "start"
    } else if (selectionStart == length && selectionEnd == length) {
      cursorPosition = "end"
    } else if (selectionStart == 0 && selectionDirection == "backward") {
      cursorPosition = "start"
    } else if (selectionEnd == length && selectionDirection == "forward") {
      cursorPosition = "end"
    }

    toElm("TextCursor",
      { selected: selectionStart !== selectionEnd
      , position: cursorPosition
      }
    );
  }
}

document.onselectionchange = selectionHandler;


Mousetrap.bind(helpers.shortcuts, function(e, s) {
  toElm("Keyboard",s);

  if(helpers.needOverride.includes(s)) {
    return false;
  }
});


Mousetrap.bind(['tab'], function(e, s) {
  document.execCommand('insertText', false, '  ')
  return false;
});

Mousetrap.bind(['shift+tab'], function(e, s) {
  return true;
});


/* === DOM manipulation === */


document.addEventListener('click', (ev) => {
  if(ev.target.nodeName == "A") {
    ev.preventDefault()
    server.openExternal(ev.target.href)
  }
})


const observer = new MutationObserver(function(mutations) {
  let isTextarea = function(node) {
    return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap"
  }

  let textareas = [];

  mutations
    .map( m => {
          [].slice.call(m.addedNodes)
            .map(n => {
              if (isTextarea(n)) {
                textareas.push(n)
              } else {
                if(n.querySelectorAll) {
                  let tareas = [].slice.call(n.querySelectorAll('textarea.edit'))
                  textareas = textareas.concat(tareas)
                }
              }
            })
        })

  if (textareas.length !== 0) {
    textareas.map(t => {
      t.oninput = editingInputHandler;
    })

    ipcRenderer.send('doc:edit-mode-toggle', true)
    jQuery(textareas).textareaAutoSize()
  } else {
    ipcRenderer.send('doc:edit-mode-toggle', false)
  }
});

const config = { childList: true, subtree: true };

observer.observe(document.body, config);
