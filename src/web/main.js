const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')
const url = require('url')
const PouchDB = require('pouchdb-browser')
const React = require('react')
const ReactDOM = require('react-dom')
const CommitsGraph = require('react-commits-graph')

const shared = require('../shared/shared')
window.Elm = require('../elm/Main')



/* === Global Variables === */

var field = null
var editing = null
var blankAutosave = null
var currentSwap = null
var saved = true
var lastCenterline = null
var lastColumnIdx = null


/* === Initializing App === */


self.gingko = Elm.Main.fullscreen(null)


/* === Database === */

self.db = new PouchDB('atreenodes16')
self.remoteCouch = 'http://localhost:5984/atreenodes16'
self.remoteDb = new PouchDB(remoteCouch)

var load = function(headOverride){
  db.get('_local/status')
    .catch(err => {
      if(err.name == "not_found") {
        return {_id: '_local/status' , status : 'bare', bare: true}
      }
    })
    .then(statusDoc => {
      db.allDocs(
        { include_docs: true
        , conflicts: true
        }).then(function (result) {
        data = result.rows.map(r => r.doc)

        var processData = function (data, type) {
          var processed = data.filter(d => d.type === type).map(d => _.omit(d, ['type','_rev']))
          var dict = {}
          if (type == "ref") {
            processed.map(d => dict[d._id] = d.value)
          } else {
            processed.map(d => dict[d._id] = _.omit(d, '_id'))
          }
          return dict
        }

        var commits = processData(data, "commit");
        var trees = processData(data, "tree");
        var refs = processData(data, "ref");
        var status = _.omit(statusDoc, '_rev')

        if(headOverride) {
          refs['heads/master'] = headOverride
        }

        var toSend = [status, { commits: commits, treeObjects: trees, refs: refs}];
        gingko.ports.load.send(toSend);
      }).catch(function (err) {
        console.log(err)
      })
    })
}

var merge = function(headOverride){
  db.allDocs(
    { include_docs: true
    , conflicts: true
    }).then(function (result) {
    data = result.rows.map(r => r.doc)

    var processData = function (data, type) {
      var processed = data.filter(d => d.type === type).map(d => _.omit(d, ['type','_rev']))
      var dict = {}
      if (type == "ref") {
        processed.map(d => dict[d._id] = d.value)
      } else {
        processed.map(d => dict[d._id] = _.omit(d, '_id'))
      }
      return dict
    }

    var commits = processData(data, "commit");
    var trees = processData(data, "tree");
    var refs = processData(data, "ref");

    if(headOverride) {
      refs['heads/master'] = headOverride
    }

    var toSend = { commits: commits, treeObjects: trees, refs: refs};
    gingko.ports.merge.send(toSend);
  }).catch(function (err) {
    console.log(err)
  })
}

load(); //initial load




/* === From Main process to Elm === */


/* === Elm Ports === */

gingko.ports.js.subscribe( function(elmdata) {
  switch (elmdata[0]) {
    case 'pull':
      remoteDb.get('heads/master').catch(err => console.log('get head before fetch error:', err))
        .then(remoteHead => {
          db.replicate.from(remoteCouch)
            .on('complete', function(info) {
              merge(remoteHead.value)
            })
        })
      break

    case 'push':
      db.replicate.to(remoteCouch)
        .on('complete', function(info) {
        })
      break
  }
})

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
      }
    })
    .then(statusDoc => {
      if(statusDoc._rev) {
        status['_rev'] = statusDoc._rev
      }

      db.get('heads/master')
        .catch(err => {
          if(err.name == "not_found") {
            return {_id: 'heads/master' , value : '', type: 'ref' }
          }
        })
        .then(headDoc => {
          newRefs = objects.refs
            .map(r => {
              if (r._id == 'heads/master') {
                headDoc['value'] = r.value
                return headDoc
              } else { return r }
            })

          var toSave = objects.commits.concat(objects.treeObjects).concat(newRefs).concat([status]);
          db.bulkDocs(toSave)
            .catch(err => {
              console.log(err)
            })
        })
    })
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



/* === Local Functions === */

var setTitleFilename = function(filepath) {
  document.title =
    filepath ? `Gingko - ${path.basename(filepath)}` : "Gingko - Untitled"
}

setSaved = bool => {
  saved = bool
  if (bool) {
    if(isNaN(saveCount)) {
      saveCount = 1
    } else {
      saveCount++
    }

    localStorage.setItem('saveCount', saveCount)
    window.Intercom('update', {"save_count": saveCount})
    maybeRequestPayment() 
  } else {
    document.title = 
      /\*/.test(document.title) ? document.title : document.title + "*"
  }
}

save = (model, success, failure) => {
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
  if (saved) {
    setSaved(false)
  }
  field = ev.target.value
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
