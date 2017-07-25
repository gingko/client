const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')

const fs = require('fs')
const path = require('path')
const mkdirp = require('mkdirp')
const {ipcRenderer, remote} = require('electron')
const {app, dialog} = remote
const querystring = require('querystring')

const PouchDB = require('pouchdb')
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


/* === Initializing App === */

var dbname = querystring.parse(window.location.search.slice(1))['dbname'] || sha1(Date.now()+machineIdSync())
var filename = querystring.parse(window.location.search.slice(1))['filename'] || "Untitled Tree"
document.title = `Gingko - ${filename}`

dbpath = path.join(app.getPath('userData'), dbname)
mkdirp.sync(dbpath)
self.db = new PouchDB(dbpath)
self.gingko = Elm.Main.fullscreen([dbname, filename])
self.socket = io.connect('http://localhost:3000')


/* === Database === */

self.remoteCouch = 'http://localhost:5984/atreenodes16'
self.remoteDb = new PouchDB(remoteCouch)


var processData = function (data, type) {
  var processed = data.filter(d => d.type === type).map(d => _.omit(d, 'type'))
  var dict = {}
  if (type == "ref") {
    processed.map(d => dict[d._id] = _.omit(d, '_id'))
  } else {
    processed.map(d => dict[d._id] = _.omit(d, ['_id','_rev']))
  }
  return dict
}


var load = function(headOverride){
  db.get('_local/status')
    .catch(err => {
      if(err.name == "not_found") {
        return {_id: '_local/status' , status : 'bare', bare: true}
      } else {
        console.log('load status error', err)
      }
    })
    .then(statusDoc => {
      status = statusDoc.status;

      db.allDocs(
        { include_docs: true
        }).then(function (result) {
        data = result.rows.map(r => r.doc)
        console.log('allDocs', data)

        var commits = processData(data, "commit");
        var trees = processData(data, "tree");
        var refs = processData(data, "ref");
        var status = _.omit(statusDoc, '_rev')

        if(headOverride) {
          refs['heads/master'] = headOverride
        }

        var toSend = [status, { commits: commits, treeObjects: trees, refs: refs}];
        console.log(toSend)
        gingko.ports.load.send(toSend);
      }).catch(function (err) {
        console.log(err)
      })
    })
}

var merge = function(local, remote){
  db.allDocs( { include_docs: true })
    .then(function (result) {
      data = result.rows.map(r => r.doc)

      var commits = processData(data, "commit");
      var trees = processData(data, "tree");
      var refs = processData(data, "ref");

      var toSend = { commits: commits, treeObjects: trees, refs: refs};
      gingko.ports.merge.send([local, remote, toSend]);
    }).catch(function (err) {
      console.log(err)
    })
}


load();



/* === From Main process to Elm === */

var collab = {}

/* === Elm Ports === */

gingko.ports.js.subscribe( function(elmdata) {
  switch (elmdata[0]) {
    case 'save':
      save(elmdata[1])
      break

    case 'save-as':
      saveAs()
      break

    case 'saved':
      changed = false
      currentFile = elmdata[1]
      document.title = `Gingko - ${path.basename(currentFile)}`
      break

    case 'open':
      open()
      break

    case 'save-and-open':
      break

    case 'save-as-and-open':
      break

    case 'pull':
      sync()
      break

    case 'push':
      push('push:elm-triggered')
      break

    case 'socket-send':
      collab = elmdata[1]
      socket.emit('collab', elmdata[1])
      break

    case 'toggle-online':
      if(elmdata[1]) {
        console.log('connect')
        socket.connect()
      } else {
        console.log('disconnect')
        socket.disconnect()
      }
      break

    case 'alert':
      alert(elmdata[1])
      break
  }
})

ipcRenderer.on('save', e => {

})

socket.on('collab', data => {
  gingko.ports.collabMsg.send(data)
})

socket.on('collab-leave', data => {
  gingko.ports.collabLeave.send(data)
})

// Fetch + Merge
var pull = function (local, remote, info) {
  db.replicate.from(remoteCouch)
    .on('complete', pullInfo => {
      if(pullInfo.docs_written > 0 && pullInfo.ok) {
        merge(local, remote)
      }
    })
}


// Push
var push = function (info) {
  db.replicate.to(remoteCouch)
}


var sync = function () {
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

gingko.ports.activateCards.subscribe(actives => {
  shared.scrollHorizontal(actives[0])
  shared.scrollColumns(actives[1])
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
  db.get('_local/status')
    .catch(err => {
      if(err.name == "not_found") {
        return {_id: '_local/status' , status : 'bare', bare: true}
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
  commitGraphData = _.sortBy(data[0].commits, 'timestamp').reverse().map(c => { return {sha: c._id, parents: c.parents}})
  selectedSha = data[1]

  var commitElement = React.createElement(CommitsGraph, {
    commits: commitGraphData,
    onClick: setHead,
    selected: selectedSha
  });

  ReactDOM.render(commitElement, document.getElementById('history'))
})

var setHead = function(sha) {
  if (sha) {
    gingko.ports.setHead.send(sha)
  }
}




/* === From Main process to Elm === */


ipcRenderer.on('attempt-save', function(e) {
  if(currentFile) {
    save(currentFile)
  } else {
    saveAs()
  }
})

ipcRenderer.on('attempt-save-as', (e) => {
  saveAs()
})


ipcRenderer.on('attempt-open', function(e) {

})





/* === Local Functions === */

save = (filepath) => {
  var ws = fs.createWriteStream(filepath)
  db.dump(ws).then( res => {
    gingko.ports.externals.send(['saved', filepath])
  }).catch( err => {
    console.log('Save error', err)
  })
}


saveAs = () => {
  var options =
    { title: 'Save As'
    , defaultPath: currentFile ? currentFile.replace('.gko', '') : path.join(app.getPath('documents'),"Untitled.gko")
    , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }

  dialog.showSaveDialog(options, function(filepath){
    if(!!filepath){
      save(filepath)
    }
  })
}

open = () => {
  dialog.showOpenDialog(
    null,
    { title: "Open File..."
    , defaultPath: currentFile ? path.dirname(currentFile) : app.getPath('documents')
    , properties: ['openFile']
    , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }
    , function(filepathToLoad) {
        if(!!filepathToLoad[0]) {
          console.log('filepathToLoad', filepathToLoad[0])
          var rs = fs.createReadStream(filepathToLoad[0])

          db.load(rs).then( res => {
            load()
          }).catch( err => {
            console.log('file load err', err)
          })
        }
      }
  )
}



// Special handling of exit case
// TODO: Find out why I can't pass app.exit as
// success callback to regular save function
saveAndExit = (model) => {
}

autosave = function(model) {
}


unsavedWarningThen = (model, success, failure) => {
}

exportToJSON = (model) => {
}

exportToMarkdown = (model) => {
}

attemptLoadFile = filepath => {
}

loadFile = (filepath, setpath) => {
}

importFile = filepath => {
}

newFile = function() {
  setTitleFilename(null)
  gingko.ports.data.send(null)
}

openDialog = function() { // TODO: add defaultPath
}

importDialog = function() {
}

clearSwap = function(filepath) {
  var file = filepath ? filepath : currentSwap
  fs.unlinkSync(file)
}

toFileFormat = model => {
  if (field !== null) {
    model = _.extend(model, {'field': field})
  } 
  return JSON.stringify(_.omit(model, 'filepath'), null, 2)
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


editingInputHandler = function(ev) {
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



var observer = new MutationObserver(function(mutations) {
  var isTextarea = function(node) {
    return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap"
  }

  var textareas = [];

  mutations
    .map( m => {
          [].slice.call(m.addedNodes)
            .map(n => {
              if (isTextarea(n)) {
                textareas.push(n)
              } else {
                if(n.querySelectorAll) {
                  var tareas = [].slice.call(n.querySelectorAll('textarea.edit'))
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
 
var config = { childList: true, subtree: true };
 
observer.observe(document.body, config);
