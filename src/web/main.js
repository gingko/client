const jQuery = require('jquery')
const _ = require('lodash')
const autosize = require('textarea-autosize')
const url = require('url')
const PouchDB = require('pouchdb-browser')

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

var db = new PouchDB('data2')
var remoteCouch = 'http://localhost:5984/kittens'

db.sync(remoteCouch, {live: true, retry: true}, (err) => console.log(err))
db.changes({since: 'now', include_docs: true, live: true, conflicts: true})
  .on('change', function (change) {
    if(change.deleted) {
      gingko.ports.externals.send(['change-deleted', change.id])
    }
    else if (change.doc._conflicts) {
      db.get(change.id, {
        open_revs: change.doc._conflicts
      })
      .then(function(responses) {
        var docs = responses
          .filter(function(response){
            return 'ok' in response
          })
          .map(function(response) {
            return response.ok
          })
          .concat(change.doc)

        var wDocs = JSON.parse(JSON.stringify(docs))

        var winning = wDocs.reduce(function(winning, doc) {
          return winning && shared.resolver(doc, winning)
        }, wDocs.pop())

        if (!winning) throw({
          error: 'conflict_resolution_failed',
          reason: 'The conflict could not be resolved, resolveFun did not return a doc'
        })

        return docs.filter(function(doc) {
          return doc._rev !== winning._rev || JSON.stringify(doc) !== JSON.stringify(winning)
        })
        .map(function(doc) {
          if (doc._rev == winning._rev) return winning

          return {
            _id: doc._id,
            _rev: doc._rev,
            _deleted: true
          }

        })
      })
      .then(function(docs) {
        return db.bulkDocs(docs)
      })
    }
    else {
      gingko.ports.change.send(
        [ change.id
        , _.mapKeys(_.omit(change.doc, ['_id']), function(val, key) {
            return key == "_rev" ? "rev" : key
          })
        ])
    }
  })
  .on('error', function (err) {
  })

shared.loadModel(db, function(data) {
  gingko.ports.nodes.send(data)
})


/* === From Main process to Elm === */


/* === Elm Ports === */

gingko.ports.message.subscribe(function(msg) {
  switch (msg[0]) {
    case 'new':
      newFile()
      break
    case 'open':
      openDialog()
      break
    case 'import':
      importDialog()
      break
    case 'save':
      shared.saveModel(db, msg[1])
      break
    case 'save-and-close':
      saveAndExit(msg[1])
      break
    case 'save-temp':
      shared.saveModel(db, msg[1])
      break
    case 'unsaved-new':
      unsavedWarningThen( msg[1]
        , newFile
        , (err) => dialog.showErrorBox("Save error:", err.message)
        )
      break;
    case 'unsaved-open':
      unsavedWarningThen( msg[1]
        , openDialog
        , (err) => dialog.showErrorBox("Save error:", err.message)
        )
      break;
    case 'undo-state-change':
      model = msg[1]
      break
    case 'confirm-cancel':
      var options =
        { type: "warning"
        , buttons: ["OK", "Cancel"]
        , title: msg[1].title
        , message: msg[1].message
        }
      dialog.showMessageBox(options, function(e) {
        if(e === 0) {
          gingko.ports.externals.send(['confirm-cancel', 'true'])
        }
      })
      break
  }
})

gingko.ports.activateCards.subscribe(actives => {
  shared.scrollHorizontal(actives[0])
  shared.scrollColumns(actives[1])
})

gingko.ports.attemptUpdate.subscribe(id => {
  var tarea = document.getElementById('card-edit-'+id)

  if (tarea === null) {
    gingko.ports.updateError.send('Textarea with id '+id+' not found.')
  } else {
    field = null
    gingko.ports.updateSuccess.send([id, tarea.value])
  }
})




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


/* === Payment Request Functions === */

maybeRequestPayment = () => {
  var t = Date.now()
  if (  isTrial
     && (saveCount > 10)
     && (isNaN(lastRequestTime) || t - lastRequestTime > 3.6e6)
     && (Math.random() < freq(t-firstRunTime))
     )
    {
      lastRequestTime = t

      if(isNaN(requestCount)) {
        requestCount = 1;
      } else {
        requestCount++
      }
      window.Intercom('update', { "request_count": requestCount })
      localStorage.setItem('requestCount', requestCount)
      localStorage.setItem('lastRequestTime', t)
    }
}

freq = tau => {
  if (tau <= 7*24*3.6e6) {
    return 0.1
  } else if (tau <= 30*24*3.6e6) {
    return 0.5
  } else {
    return 0.8
  }
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



var shortcuts = [ 'mod+enter'
                , 'enter'
                , 'esc'
                , 'mod+backspace'
                , 'mod+j'
                , 'mod+k'
                , 'mod+l'
                , 'mod+down'
                , 'mod+up'
                , 'mod+right'
                , 'h'
                , 'j'
                , 'k'
                , 'l'
                , 'left'
                , 'down'
                , 'up'
                , 'right'
                , 'alt+left'
                , 'alt+down'
                , 'alt+up'
                , 'alt+right'
                , '['
                , ']'
                , 'mod+z'
                , 'mod+r'
                , 'mod+s'
                , 'mod+x' // debug command
                ];

var needOverride= [ 'mod+j'
                  , 'mod+l'
                  , 'mod+s'  
                  , 'mod+r'
                  , 'alt+left'
                  , 'alt+right'
                  ];
                    
Mousetrap.bind(shortcuts, function(e, s) {
  gingko.ports.externals.send(['keyboard', s]);

  if(needOverride.includes(s)) {
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
