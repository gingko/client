const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')

const fs = require('fs')
const path = require('path')
const mkdirp = require('mkdirp')
const {ipcRenderer, remote} = require('electron')
const {app, dialog} = remote
const querystring = require('querystring')

import PouchDB from "pouchdb";
const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)
PouchDB.adapter('writableStream', replicationStream.adapters.writableStream)

const sha1 = require('sha1')
const machineIdSync = require('electron-machine-id').machineIdSync

const React = require('react')
const ReactDOM = require('react-dom')
const CommitsGraph = require('react-commits-graph')
const io = require('socket.io-client')

const shared = require('./shared')
window.Elm = require('../elm/Main')



/* === Global Variables === */

var currentFile = null
var changed = false
var field = null
var editing = null
var currentSwap = null
var lastCenterline = null
var lastColumnIdx = null
var collab = {}




/* === Initializing App === */

var dbname = querystring.parse(window.location.search.slice(1))['dbname'] || sha1(Date.now()+machineIdSync())
var filename = querystring.parse(window.location.search.slice(1))['filename'] || "Untitled Tree"
document.title = `${filename} - Gingko`

var dbpath = path.join(app.getPath('userData'), dbname)
mkdirp.sync(dbpath)
self.db = new PouchDB(dbpath)
self.gingko = Elm.Main.fullscreen()
self.socket = io.connect('http://localhost:3000')




/* === Elm to JS Ports === */

const update = (msg, arg) => {
  let cases =
    { 'new': () => {
        let clearDb = () => {
          dbname = sha1(Date.now()+machineIdSync())
          filename = "Untitled Tree"
          document.title = `${filename} - Gingko`

          dbpath = path.join(app.getPath('userData'), dbname)
          mkdirp.sync(dbpath)
          self.db = new PouchDB(dbpath)
          gingko.ports.externals.send(['new', ''])
        }

        if(changed) {
          saveConfirmation(arg).then( () => {
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

    , 'save': () =>
        save(arg)
          .then( filepath =>
            gingko.ports.externals.send(['saved', filepath])
          )

    , 'save-as': () =>
        saveAs()
          .then( filepath =>
            gingko.ports.externals.send(['saved', filepath])
          )

    , 'saved': () => {
        changed = false
        currentFile = arg
        document.title = `${path.basename(currentFile)} - Gingko`
      }

    , 'open': openDialog

    , 'save-and-open': () => {
        saveConfirmation(arg).then(openDialog)
      }

    , 'save-as-and-open': () => {
        saveConfirmation(null).then(openDialog)
      }

    , 'pull': sync

    , 'push': push

    , 'socket-send': () => {
        collab = arg
        socket.emit('collab', arg)
      }
  }

  try {
    cases[msg]()
  } catch(err) {
    console.log('elmCases failed:', msg, arg)
  }
}


gingko.ports.js.subscribe(function(elmdata) {
  update(elmdata[0], elmdata[1])
})


gingko.ports.getText.subscribe(id => {
  var tarea = document.getElementById('card-edit-'+id)

  if (tarea === null) {
    gingko.ports.updateError.send('Textarea with id '+id+' not found.')
  } else {
    gingko.ports.updateContent.send([id, tarea.value])
  }
})


gingko.ports.saveObjects.subscribe(data => {
  var status = data[0]
  var objects = data[1]
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

      var toSave = objects.commits.concat(objects.treeObjects).concat(objects.refs).concat([status]);
      db.bulkDocs(toSave)
        .catch(err => {
          console.log(err)
        })
        .then(responses => {
          var head = responses.filter(r => r.id == "heads/master")[0]
          if (head.ok) {
            gingko.ports.setHeadRev.send(head.rev)
          } else {
            console.log('head not ok', head)
          }
        })
    })
})


gingko.ports.saveLocal.subscribe(data => {
  console.log(data)
})


gingko.ports.updateCommits.subscribe(function(data) {
  let commitGraphData = _.sortBy(data[0].commits, 'timestamp').reverse().map(c => { return {sha: c._id, parents: c.parents}})
  let selectedSha = data[1]

  var commitElement = React.createElement(CommitsGraph, {
    commits: commitGraphData,
    onClick: setHead,
    selected: selectedSha
  });

  ReactDOM.render(commitElement, document.getElementById('history'))
})


gingko.ports.activateCards.subscribe(actives => {
  shared.scrollHorizontal(actives[0])
  shared.scrollColumns(actives[1])
})




/* === JS to Elm Ports === */

ipcRenderer.on('menu-new', () => update('new'))
ipcRenderer.on('menu-open', () => update('open'))
ipcRenderer.on('menu-save', () => update('save', currentFile))
ipcRenderer.on('menu-save-as', () => update('save-as'))

socket.on('collab', data => gingko.ports.collabMsg.send(data))
socket.on('collab-leave', data => gingko.ports.collabLeave.send(data))




/* === Database === */

self.remoteCouch = 'http://localhost:5984/atreenodes16'
self.remoteDb = new PouchDB(remoteCouch)


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
        }

        let toSend = [filepath, [status, { commits: commits, treeObjects: trees, refs: refs}]];
        gingko.ports.load.send(toSend);
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
      gingko.ports.merge.send([local, remote, toSend]);
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
    gingko.ports.setHead.send(sha)
  }
}




/* === Local Functions === */

const save = (filepath) => {
  return new Promise(
    (resolve, reject) => {
      let ws = fs.createWriteStream(filepath)
      db.dump(ws).then( res => {
        if (res.ok) {
          resolve(filepath)
        } else {
          reject(res)
        }
      }).catch( err => {
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
        var filepathToLoad = filepathArray[0]
        if(!!filepathToLoad) {
          var rs = fs.createReadStream(filepathToLoad)
          db.destroy().then( res => {
            if (res.ok) {
              dbpath = path.join(app.getPath('userData'), sha1(filepathToLoad))
              mkdirp.sync(dbpath)
              self.db = new PouchDB(dbpath)

              db.load(rs).then( res => {
                if (res.ok) {
                  currentFile = filepathToLoad
                  changed = false
                  document.title = `${path.basename(currentFile)} - Gingko`
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
      }
  )
}




/* === DOM Events and Handlers === */

document.ondragover = document.ondrop = (ev) => {
  ev.preventDefault()
}

document.body.ondrop = (ev) => {
  //saveConfirmAndThen(attemptLoadFile(ev.dataTransfer.files[0].path))
  ev.preventDefault()
}

window.onresize = () => {
  if (lastCenterline) { scrollColumns(lastCenterline) }
  if (lastColumnIdx) { scrollHorizontal(lastColumnIdx) }
}


const editingInputHandler = function(ev) {
  if (!changed) {
    changed = true
    if (!/\*/.test(document.title)) {
      document.title = document.title + "*"
    }
    gingko.ports.externals.send(['changed', ''])
  }
  collab.field = ev.target.value
  socket.emit('collab', collab)
}



Mousetrap.bind(shared.shortcuts, function(e, s) {
  gingko.ports.keyboard.send(s);

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
