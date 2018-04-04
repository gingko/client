const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')
const Mousetrap = require('mousetrap')

const fs = require('fs')
const getHash = require('hash-stream')
const path = require('path')
const {ipcRenderer, remote, webFrame, shell} = require('electron')
const {app, dialog} = remote
const querystring = require('querystring')
const MemoryStream = require('memorystream')
const Store = require('electron-store')

import PouchDB from "pouchdb-browser";

const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)
PouchDB.adapter('writableStream', replicationStream.adapters.writableStream)
import memoryAdapter from "pouchdb-adapter-memory";
PouchDB.plugin(memoryAdapter)

const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync

const React = require('react')
const ReactDOM = require('react-dom')
const CommitsGraph = require('react-commits-graph')
const io = require('socket.io-client')

const shared = require('./shared')
window.Elm = require('../elm/Main')



/* === Global Variables === */

var currentFile = null
var changed = false
var saveInProgress = false
var field = null
var editing = null
var currentSwap = null
var lastCenterline = null
var lastColumnIdx = null
var collab = {}


const userStore = new Store({name: "config"})
userStore.getWithDefault = function (key, def) {
  let val = userStore.get(key);
  if (typeof val === "undefined") {
    return def;
  } else {
    return val;
  }
}




/* === Initializing App === */

console.log('Gingko version', app.getVersion())

var firstRun = userStore.getWithDefault('first-run', true)

var dbname = querystring.parse(window.location.search.slice(1))['dbname'] || sha1(Date.now()+machineIdSync())
var filename = querystring.parse(window.location.search.slice(1))['filename'] || "Untitled Tree"
document.title = `${filename} - Gingko`

var dbpath = path.join(app.getPath('userData'), dbname)
self.db = new PouchDB(dbpath, {adapter: 'memory'})

var initFlags =
  [ process.platform === "darwin"
  , userStore.getWithDefault('shortcut-tray-is-open', true)
  , userStore.getWithDefault('video-modal-is-open', false)
  ]

self.gingko = Elm.Main.fullscreen(initFlags)
self.socket = io.connect('http://localhost:3000')

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
    { 'Alert': () => { alert(data) }

    , 'ActivateCards': () => {
        setLastActive(currentFile, data[0])
        shared.scrollHorizontal(data[1])
        shared.scrollColumns(data[2])
      }

    , 'GetText': () => {
        let id = data
        let tarea = document.getElementById('card-edit-'+id)

        if (tarea === null) {
          // TODO: replace this with proper logging.
          gingko.ports.updateError.send('Textarea with id '+id+' not found.')
        } else {
          gingko.ports.infoForElm.send({tag: 'UpdateContent', data: [id, tarea.value]})
        }
      }

    , 'TextSurround': () => {
        let id = data[0]
        let surroundString = data[1]
        let tarea = document.getElementById('card-edit-'+id)

        if (tarea === null) {
          // TODO: replace this with proper logging.
          gingko.ports.updateError.send('Textarea with id '+id+' not found.')
        } else {
          let start = tarea.selectionStart
          let end = tarea.selectionEnd
          if (start !== end) {
            let text = tarea.value.slice(start, end)
            let modifiedText = surroundString + text + surroundString
            document.execCommand('insertText', true, modifiedText)
          }
        }
      }

    , 'SaveObjects': () => {
        let status = data[0]
        let objects = data[1]
        db.get('status')
          .catch(err => {
            if(err.name == "not_found") {
              return {_id: 'status' , status : 'bare', bare: true}
            } else {
              console.log('load status error', err)
            }
          })
          .then(statusDoc => {
            if(statusDoc._rev) {
              status['_rev'] = statusDoc._rev
            }

            let toSave = objects.commits.concat(objects.treeObjects).concat(objects.refs).concat([status]);
            db.bulkDocs(toSave)
              .catch(err => {
                console.log(err)
              })
              .then(responses => {
                let head = responses.filter(r => r.id == "heads/master")[0]
                if (head.ok) {
                  gingko.ports.infoForElm.send({tag: 'SetHeadRev', data: head.rev})
                } else {
                  console.log('head not ok', head)
                }
              })
          })
      }

    , 'UpdateCommits': () => {
        let commitGraphData = _.sortBy(data[0].commits, 'timestamp').reverse().map(c => { return {sha: c._id, parents: c.parents}})
        let selectedSha = data[1]

        let commitElement = React.createElement(CommitsGraph, {
          commits: commitGraphData,
          onClick: setHead,
          selected: selectedSha
        });

        //ReactDOM.render(commitElement, document.getElementById('history'))
    }

    , 'ConfirmCancel': () => {
        let tarea = document.getElementById('card-edit-'+data[0])

        if (tarea === null) {
          console.log('tarea not found')
        } else {
          if(tarea.value === data[1]) {
            gingko.ports.infoForElm.send({tag: 'CancelCardConfirmed', data: null})
          } else if (confirm('Are you sure you want to cancel your changes?')) {
            gingko.ports.infoForElm.send({tag: 'CancelCardConfirmed', data: null})
          }
        }
      }

    , 'ColumnNumberChange': () => {
        ipcRenderer.send('column-number-change', data)
      }

    , 'New': () => {
        let clearDb = () => {
          dbname = sha1(Date.now()+machineIdSync())
          setFileState(false, null)

          dbpath = path.join(app.getPath('userData'), dbname)
          self.db = new PouchDB(dbpath, {adapter: 'memory'})
          gingko.ports.infoForElm.send({tag: 'Reset', data: null})
        }

        if(saveInProgress) {
          _.delay(update, 200, 'New')
        } else {
          if(changed) {
            saveConfirmation(data).then( () => {
              db.destroy().then( res => {
                if (res.ok) {
                  clearDb()
                }
              })
            })
          } else {
            clearDb()
          }
        }
      }

    , 'Open': () => {
        if (saveInProgress) {
          _.delay(update, 200, 'Open')
        } else {
          if (changed) {
            saveConfirmation(data).then(openDialog)
          } else {
            openDialog()
          }
        }
      }

    , 'Import': () => {
        if (saveInProgress) {
          _.delay(update, 200, 'Import')
        } else {
          if (changed) {
            saveConfirmation(data).then(importDialog)
          } else {
            importDialog()
          }
        }
      }

    , 'Save': () =>
        save(data)
          .then( filepath =>
            gingko.ports.infoForElm.send({tag:'Saved', data: filepath})
          )


    , 'SaveAs': () =>
        saveAs()
          .then( filepath =>
            gingko.ports.infoForElm.send({tag:'Saved', data: filepath})
          )

    , 'SaveAndClose': () => {
        if(saveInProgress) {
          _.delay(update, 200, 'SaveAndClose')
        } else {
          saveConfirmation(data).then(app.exit)
        }
      }

    , 'ExportJSON': () => {
        exportJson(data)
      }

    , 'ExportTXT': () => {
        exportTxt(data)
      }

    , 'ExportTXTColumn': () => {
        exportTxt(data)
      }

    , 'SetSaved': () =>
        setFileState(false, data)

    , 'SetVideoModal': () => {
        userStore.set('video-modal-is-open', data)
      }

    , 'SetShortcutTray': () => {
        userStore.set('shortcut-tray-is-open', data)
      }

    , 'SetChanged': () => {
        setFileState(true, currentFile)
      }

    , 'Pull': sync

    , 'Push': push

    , 'SocketSend': () => {
        collab = data
        socket.emit('collab', data)
      }

    , 'ConsoleLogRequested': () =>
        console.log(data)

    }

  try {
    cases[msg]()
  } catch(err) {
    console.log('elmCases one-port failed:', err, msg, data)
  }
}


gingko.ports.infoForOutside.subscribe(function(elmdata) {
  update(elmdata.tag, elmdata.data)
})




/* === JS to Elm Ports === */

ipcRenderer.on('menu-new', () => update('New'))
ipcRenderer.on('menu-open', () => update('Open'))
ipcRenderer.on('menu-import-json', () => update('Import'))
ipcRenderer.on('menu-export-json', () => gingko.ports.infoForElm.send({tag: 'DoExportJSON', data: null }))
ipcRenderer.on('menu-export-txt', () => gingko.ports.infoForElm.send({tag: 'DoExportTXT', data: null }))
ipcRenderer.on('menu-export-txt-current', () => gingko.ports.infoForElm.send({tag: 'DoExportTXTCurrent', data: null }))
ipcRenderer.on('menu-export-txt-column', (e, msg) => gingko.ports.infoForElm.send({tag: 'DoExportTXT', data: msg }))
ipcRenderer.on('menu-save', () => gingko.ports.infoForElm.send({tag: 'Keyboard', data: 'mod+s'}))
ipcRenderer.on('menu-save-as', () => update('SaveAs'))
ipcRenderer.on('zoomin', e => { webFrame.setZoomLevel(webFrame.getZoomLevel() + 1) })
ipcRenderer.on('zoomout', e => { webFrame.setZoomLevel(webFrame.getZoomLevel() - 1) })
ipcRenderer.on('resetzoom', e => { webFrame.setZoomLevel(0) })
ipcRenderer.on('menu-view-videos', () => gingko.ports.infoForElm.send({tag: 'ViewVideos', data: null }))
ipcRenderer.on('menu-contact-support', () => { if(crisp_loaded) { $crisp.push(['do', 'chat:open']); $crisp.push(['do', 'chat:show']); } else { shell.openExternal('mailto:adriano@gingkoapp.com') } } )
ipcRenderer.on('main-save-and-close', () => update('SaveAndClose', currentFile))

socket.on('collab', data => gingko.ports.infoForElm.send({tag: 'RecvCollabState', data: data}))
socket.on('collab-leave', data => gingko.ports.infoForElm.send({tag: 'CollaboratorDisconnected', data: data}))






/* === Database === */

const processData = function (data, type) {
  var processed = data.filter(d => d.type === type).map(d => _.omit(d, 'type'))
  var dict = {}
  if (type == "ref") {
    processed.map(d => dict[d._id] = _.omit(d, '_id'))
  } else {
    processed.map(d => dict[d._id] = _.omit(d, ['_id','_rev']))
  }
  return dict
}


const load = function(filepath, headOverride){
  db.get('status')
    .catch(err => {
      if(err.name == "not_found") {
        console.log('load status not found. Setting to "bare".')
        return {_id: 'status' , status : 'bare', bare: true}
      } else {
        console.log('load status error', err)
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
          refs['heads/master'] = { value: lastCommit, ancestors: [], _rev: "" }
          console.log('recovered status', status)
          console.log('refs recovered', refs)
        }

        let toSend = [filepath, [status, { commits: commits, treeObjects: trees, refs: refs}], getLastActive(filepath)];
        gingko.ports.infoForElm.send({tag: "Load", data: toSend});
      }).catch(function (err) {
        console.log(err)
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
      gingko.ports.infoForElm.send({tag: "Merge", data: [local, remote, toSend]});
    }).catch(function (err) {
      console.log(err)
    })
}


const pull = function (local, remote, info) {
  db.replicate.from(remoteCouch)
    .on('complete', pullInfo => {
      if(pullInfo.docs_written > 0 && pullInfo.ok) {
        merge(local, remote)
      }
    })
}


const push = function () {
  db.replicate.to(remoteCouch)
}


const sync = function () {
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


const setHead = function(sha) {
  if (sha) {
    gingko.ports.infoForElm.send({tag: 'CheckoutCommit', data: sha})
  }
}




/* === Local Functions === */

self.save = (filepath) => {
  return new Promise(
    (resolve, reject) => {
      let swapfilepath = filepath + '.swp'
      let filewriteStream = fs.createWriteStream(swapfilepath)
      let memStream = new MemoryStream();
      saveInProgress = true

      db.dump(memStream).then( res => {
        if (res.ok) {
          memStream.pipe(filewriteStream)

          var streamHash;
          var swapfileHash;

          getHash(memStream, 'sha1', (err, hash) => {
            streamHash = hash.toString('base64')
            getHash(swapfilepath, 'sha1', (err, fhash) => {
              swapfileHash = fhash.toString('base64')

              if (streamHash !== swapfileHash) {
                throw new Error('File integrity check failed.')
              } else {
                // Successful save and copy
                saveInProgress = false
                fs.copyFile(swapfilepath, filepath, (copyErr) => {
                  if (copyErr) {
                    throw copyErr;
                  } else {
                    fs.unlink(swapfilepath, (delErr) => {
                      if (delErr) throw delErr;
                    })
                  }
                })
                resolve(filepath)
              }
            })
          })
        } else {
          saveInProgress = false
          reject(res)
        }
      }).catch( err => {
        saveInProgress = false
        reject(err)
      })
    }
  )
}


const saveAs = () => {
  return new Promise(
    (resolve, reject) => {
      var options =
        { title: 'Save As'
        , defaultPath: currentFile ? currentFile.replace('.gko', '') : path.join(app.getPath('documents'),"Untitled.gko")
        , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                    , {name: 'All Files', extensions: ['*']}
                    ]
        }

      dialog.showSaveDialog(options, function(filepath){
        if(!!filepath){
          resolve(save(filepath))
        } else {
          reject(new Error('no save path chosen'))
        }
      })
    }
  )
}



const saveConfirmation = (filepath) => {
  return new Promise(
    (resolve, reject) => {
      let options =
        { title: "Save changes"
        , message: "Save changes before closing?"
        , buttons: ["Close Without Saving", "Cancel", "Save"]
        , defaultId: 2
        }
      let choice = dialog.showMessageBox(options)

      if (choice == 0) {
        resolve(filepath)
      } else if (choice == 2) {
        if(filepath !== null) {
          resolve(save(filepath))
        } else {
          resolve(saveAs())
        }
      }
    }
  )
}


const exportJson = (data) => {
  return new Promise(
    (resolve, reject) => {
      var options =
        { title: 'Export JSON'
        , defaultPath: currentFile ? currentFile.replace('.gko', '') : path.join(app.getPath('documents'),"Untitled.json")
        , filters:  [ {name: 'Gingko JSON (*.json)', extensions: ['json']}
                    , {name: 'All Files', extensions: ['*']}
                    ]
        }

      dialog.showSaveDialog(options, function(filepath){
        if(!!filepath){
          fs.writeFile(filepath, JSON.stringify(data, undefined, 2), (err) => {
            if (err) reject(new Error('export-json writeFile failed'))
            resolve(data)
          })
        } else {
          reject(new Error('no export path chosen'))
        }
      })
    }
  )
}

const exportTxt = (data) => {
  return new Promise(
    (resolve, reject) => {
      
      if (data && typeof data.replace === 'function') {
        data = (process.platform === "win32") ? data.replace(/\n/g, '\r\n') : data;
      } else {
        reject(new Error('invalid data sent for export'))
      }

      var options =
        { title: 'Export TXT'
        , defaultPath: currentFile ? currentFile.replace('.gko', '') : path.join(app.getPath('documents'),"Untitled.txt")
        , filters:  [ {name: 'Text File', extensions: ['txt']}
                    , {name: 'All Files', extensions: ['*']}
                    ]
        }

      dialog.showSaveDialog(options, function(filepath){
        if(!!filepath){
          fs.writeFile(filepath, data, (err) => {
            if (err) reject(new Error('export-txt writeFile failed'))
            resolve(data)
          })
        } else {
          reject(new Error('no export path chosen'))
        }
      })
    }
  )
}


const openDialog = () => {
  dialog.showOpenDialog(
    null,
    { title: "Open File..."
    , defaultPath: currentFile ? path.dirname(currentFile) : app.getPath('documents')
    , properties: ['openFile']
    , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }
    , function(filepathArray) {
        if(Array.isArray(filepathArray) && filepathArray.length >= 0) {
          var filepathToLoad = filepathArray[0]
          if(!!filepathToLoad) {
            loadFile(filepathToLoad)
          }
        }
      }
  )
}

const importDialog = () => {
  dialog.showOpenDialog(
    null,
    { title: "Import JSON File..."
    , defaultPath: currentFile ? path.dirname(currentFile) : app.getPath('documents')
    , properties: ['openFile']
    , filters:  [ {name: 'Gingko JSON files (*.json)', extensions: ['json']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }
    , function(filepathArray) {
        var filepathToImport = filepathArray[0]
        if(!!filepathToImport) {
          importFile(filepathToImport)
        }
      }
  )
}


const loadFile = (filepathToLoad) => {
  var rs = fs.createReadStream(filepathToLoad)

  db.destroy().then( res => {
    if (res.ok) {
      dbpath = path.join(app.getPath('userData'), sha1(filepathToLoad))
      self.db = new PouchDB(dbpath, {adapter: 'memory'})

      db.load(rs).then( res => {
        if (res.ok) {
          setFileState(false, filepathToLoad)
          load(filepathToLoad)
        } else {
          console.log('db.load res is', res)
        }
      }).catch( err => {
        console.log('file load err', err)
      })
    }
  })
}


const importFile = (filepathToImport) => {
  fs.readFile(filepathToImport, (err, data) => {

    let nextId = 1

    let seed =
      JSON.parse(
        data.toString()
            .replace( /{(\s*)"content":/g
                    , s => {
                        return `{"id":"${nextId++}","content":`
                      }
                    )
      )

    let newRoot =
      seed.length == 1
        ?
          { id: "0"
          , content: seed[0].content
          , children: seed[0].children
          }
        :
          { id: "0"
          , content: path.basename(filepathToImport)
          , children: seed
          }

    db.destroy().then( res => {
      if (res.ok) {
        dbpath = path.join(app.getPath('userData'), sha1(filepathToImport))
        self.db = new PouchDB(dbpath, {adapter: 'memory'})

        document.title = `${path.basename(filepathToImport)} - Gingko`
        setFileState(true, null)
        gingko.ports.infoForElm.send({tag: 'ImportJSON', data :newRoot})
      }
    })
  })
}


function setLastActive (filename, lastActiveCard) {
  if (filename !== null) {
    userStore.set(`last-active-cards.${filename}`, lastActiveCard);
  }
}


function getLastActive (filename) {
  let lastActiveCard = userStore.get(`last-active-cards.${filename}`)
  if (typeof lastActiveCard === "undefined") {
    return null
  } else {
    return lastActiveCard
  }
}




/* === DOM Events and Handlers === */

// Prevent default events, for file dragging.
document.ondragover = document.ondrop = (ev) => {
  ev.preventDefault()
}

window.onresize = () => {
  if (lastCenterline) { scrollColumns(lastCenterline) }
  if (lastColumnIdx) { scrollHorizontal(lastColumnIdx) }
}


const setFileState = function(bool, newpath) {
  if (bool) {
    changed = true
    if (!/\*/.test(document.title)) {
      document.title = "*" + document.title
    }
    gingko.ports.infoForElm.send({ tag: 'Changed', data: null })
  } else {
    changed = false
    currentFile = newpath
    document.title = newpath ? `${path.basename(currentFile)} - Gingko` : "Untitled Tree - Gingko"
  }

  ipcRenderer.send('changed', bool)
}


const editingInputHandler = function(ev) {
  if (!changed) {
    setFileState(true, currentFile)
  }
  collab.field = ev.target.value
  socket.emit('collab', collab)
}



Mousetrap.bind(shared.shortcuts, function(e, s) {
  gingko.ports.infoForElm.send({tag: 'Keyboard', data: s});

  if(shared.needOverride.includes(s)) {
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
    shell.openExternal(ev.target.href)
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
      if(editing == t.id.split('-')[2] && field !== null) {
        t.value = field
        t.focus()
      }
      t.oninput = editingInputHandler;
    })
    jQuery(textareas).textareaAutoSize()
  }
});

const config = { childList: true, subtree: true };

observer.observe(document.body, config);
