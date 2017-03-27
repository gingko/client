var jQuery = require('jquery')
var _ = require('underscore')
var autosize = require('textarea-autosize')
const fs = require('mz/fs')
const path = require('path')
const url = require('url')
const {ipcRenderer, webFrame, remote, shell} = require('electron')
const machineIdSync = require('electron-machine-id').machineIdSync
const app = remote.app
const dialog = remote.dialog
const Menu = remote.Menu

const shared = require('../shared/shared')
window.Elm = require('../elm/Main')



/* === Global Variables === */

var gingko
var field = null
var editing = null
var blankAutosave = null
var currentSwap = null
var saved = true
var editSubMenu = Menu.getApplicationMenu().items[1].submenu;
var lastCenterline = null
var lastColumnIdx = null


/* === Config Loading === */

var firstRunTime = Number.parseInt(localStorage.getItem('firstRunTime'))
var lastRequestTime = Number.parseInt(localStorage.getItem('lastRequestTime'))
var isTrial = JSON.parse(localStorage.getItem('isTrial'))
var saveCount = Number.parseInt(JSON.parse(localStorage.getItem('saveCount')))
var requestCount = Number.parseInt(JSON.parse(localStorage.getItem('requestCount')))
var email = localStorage.getItem('email')
var name = localStorage.getItem('name')
var machineId = localStorage.getItem('machineId')

var query = url.parse(location.toString(), true).query;

if((query.name && query.email) && !(!!email && !!name)) {
  name = query.name
  email = query.email
  localStorage.setItem('name', name)
  localStorage.setItem('email', email)
}

if (isNaN(firstRunTime)) {
  firstRunTime = Date.now()
  localStorage.setItem('firstRunTime', firstRunTime)
}
if (isTrial == null) {
  isTrial = true
  localStorage.setItem('isTrial', true)
}
if (machineId == null) {
  machineId = machineIdSync().substr(0,6)
  localStorage.setItem('machineId', machineId)
}


/* === Initializing App === */

gingko =  Elm.Main.fullscreen(null)

shared.loadModel(function(data) {
  gingko.ports.data.send(data.doc.model)
})


/* === From Main process to Elm === */

ipcRenderer.on('open-file', function(e) {
  console.log(e)
})

ipcRenderer.on('attempt-new', function(e) {
  gingko.ports.externals.send(['attempt-new', ''])
})


ipcRenderer.on('attempt-open', function(e) {
  gingko.ports.externals.send(['attempt-open', ''])
})

ipcRenderer.on('attempt-import', function(e) {
  gingko.ports.externals.send(['attempt-import', ''])
})

ipcRenderer.on('attempt-save', function(e) {
  gingko.ports.externals.send(['attempt-save', ''])
})

ipcRenderer.on('attempt-save-as', function(e) {
  gingko.ports.externals.send(['attempt-save-as', ''])
})

ipcRenderer.on('clear-swap', function (e) {
  clearSwap()
})

ipcRenderer.on('attempt-save-and-close', function (e) {
  gingko.ports.externals.send(['attempt-save-and-close', ''])
})

ipcRenderer.on('export-as-json', function(e) {
  gingko.ports.externals.send(['export-as-json', ''])
})

ipcRenderer.on('export-as-markdown', function(e) {
  gingko.ports.externals.send(['export-as-markdown', ''])
})


ipcRenderer.on('undo', function (e) {
  gingko.ports.externals.send(['keyboard','mod+z'])
})
ipcRenderer.on('redo', function (e) {
  gingko.ports.externals.send(['keyboard','mod+r'])
})


ipcRenderer.on('zoomin', e => {
  webFrame.setZoomLevel(webFrame.getZoomLevel() + 1)
})
ipcRenderer.on('zoomout', e => {
  webFrame.setZoomLevel(webFrame.getZoomLevel() - 1)
})
ipcRenderer.on('resetzoom', e => {
  webFrame.setZoomLevel(0)
})

ipcRenderer.on('contact-support', e => {
})

ipcRenderer.on('id-info', (e, msg) => {
  name = msg[0]
  email = msg[1]
  localStorage.setItem('name', name)
  localStorage.setItem('email', email)
})

ipcRenderer.on('serial-success', e => {
  isTrial = false
  localStorage.setItem('isTrial', false)
})


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
      save( msg[1]
          , (path) => {
              gingko.ports.externals.send(['save-success', path]);
              setSaved(true);
              setTitleFilename(path);
            }
          , (err) => dialog.showErrorBox("Save error:", err.message)
          )
      break
    case 'save-and-close':
      saveAndExit(msg[1])
      break
    case 'save-temp':
      field = null
      setSaved(false)
      autosave(msg[1])
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
      undoRedoMenuState(model.treePast, model.treeFuture)
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
  ipcRenderer.send('saved', bool)
  if (bool) { 
    if(isNaN(saveCount)) {
      saveCount = 1
    } else {
      saveCount++
    }

    localStorage.setItem('saveCount', saveCount)
    maybeRequestPayment() 
  } else {
    document.title = 
      /\*/.test(document.title) ? document.title : document.title + "*"
  }
}

save = (model, success, failure) => {
  if (model.filepath) {
    fs.writeFile(model.filepath, toFileFormat(model))
      .then(success(model.filepath))
      .catch(failure)
  } else {
    var options =
      { title: 'Save As'
      , defaultPath: model.filepath ? `${app.getPath('documents')}/../${model.filepath.replace('.gko','')}` : `${app.getPath('documents')}/../Untitled.gko`
      , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                  , {name: 'All Files', extensions: ['*']}
                  ]
      };

    dialog.showSaveDialog(options, function(path){
      if(!!path){
        fs.writeFile(path, toFileFormat(model))
          .then(success(path))
          .catch(failure)
      }
    });
  }
}

// Special handling of exit case
// TODO: Find out why I can't pass app.exit as
// success callback to regular save function
saveAndExit = (model) => {
  if (model.filepath) {
    fs.writeFile(model.filepath, toFileFormat(model))
      .then(app.exit)
      .catch((err) => dialog.showErrorBox("Save error:", err.message))
  } else {
    var options =
      { title: 'Save As'
      , defaultPath: model.filepath ? `${app.getPath('documents')}/../${model.filepath.replace('.gko','')}` : `${app.getPath('documents')}/../Untitled.gko`
      , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                  , {name: 'All Files', extensions: ['*']}
                  ]
      };

    dialog.showSaveDialog(options, function(path){
      if(!!path){
        fs.writeFile(path, toFileFormat(model))
          .then(app.exit)
          .catch((err) => dialog.showErrorBox("Save error:", err.message))
      }
    });
  }
}

autosave = function(model) {
  if (model.filepath) {
    currentSwap =
      model.filepath.replace('.gko','.gko.swp')
  } else {
    blankAutosave =
      blankAutosave
        ? blankAutosave
        : Date.now()

    currentSwap =
      `${app.getPath('documents')}/Untitled-${blankAutosave}.gko.swp`

    localStorage.setItem('autosave', currentSwap) // TODO: warn when this exists.
  }

  fs.writeFile(currentSwap, toFileFormat(model), function(err){
    if(err) {
      dialog.showErrorBox("Autosave error.", err.message)
    } 
  });
}


unsavedWarningThen = (model, success, failure) => {
  var options =
    { title: "Save changes"
    , message: "Save changes before closing?"
    , buttons: ["Close Without Saving", "Cancel", "Save"]
    , defaultId: 2
    }

  var choice = dialog.showMessageBox(options)

  if (choice == 0) {
    success();
  } else if (choice == 2) {
    save(model, success, failure);
  }
}

exportToJSON = (model) => {
  var strip = function(tree) {
    return {"content": tree.content, "children": tree.children.map(strip)}
  }
  
  var options =
    { title: 'Export JSON'
    , defaultPath: model.filepath ? `${app.getPath('documents')}/../${model.filepath.replace('.gko','')}.json` : `${app.getPath('documents')}/../Untitled.json`
    , filters:  [ {name: 'JSON Files', extensions: ['json']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }

  dialog.showSaveDialog(options, function(e){
    if(!!e) {
      fs.writeFile(e, JSON.stringify([strip(model.tree)], null, 2), function(err){ 
        if(err) { 
          dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
          console.log(err.message)
        }
      })
    }
  })
}

exportToMarkdown = (model) => {
  var flattenTree = function(tree, depth, strings) {
    if (tree.children.length == 0) {
      return strings.concat([addHeading(depth, tree.content)])
    } else {
      return strings.concat([addHeading(depth, tree.content)], _.flatten(tree.children.map(function(c){return flattenTree(c, depth+1,[])})))
    }
  }

  var addHeading = function(depth, content) {
    if(content.startsWith("#")){
      return content
    } else {
      return "#".repeat(Math.min(6,depth+1)) + " " + content
    }
  }

  var options =
    { title: 'Export Markdown (txt)'
    , defaultPath: model.filepath ? `${app.getPath('documents')}/../${model.filepath.replace('.gko','')}.txt` : `${app.getPath('documents')}/../Untitled.txt`
    , filters:  [ {name: 'Text Files', extensions: ['txt']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }

  dialog.showSaveDialog(options, function(e){
    if(!!e) {
      fs.writeFile(e, flattenTree(model.tree, 0, []).join("\n\n"), function(err){
        if(err) { 
          dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
          console.log(err.message)
        }
      })
    }
  })
}

attemptLoadFile = filepath => {
  var swapFilepath =
    filepath.replace('.gko', '.gko.swp')

  fs.access(swapFilepath, (err) => {
    if (err) {
      loadFile(filepath)
    } else {
      var options =
        { type: "warning"
        , buttons: ["Yes", "No"]
        , title: "Recover changes?"
        , message: "Unsaved changes were found. Would you like to recover them?"
        }
      dialog.showMessageBox(options, function(e) {
        if(e === 0) {
          loadFile(swapFilepath, filepath)
        } else {
          loadFile(filepath)
        }
      })
    }
  })
}

loadFile = (filepath, setpath) => {
  fs.readFile(filepath, (err, data) => {
    if (err) throw err;
    setTitleFilename(setpath ? setpath : filepath)
    model = _.extend(JSON.parse(data), { filepath: setpath ? setpath : filepath })
    if (model.field !== undefined) {
      console.log('model.field', model.field)
      field = model.field
      model = _.omit(model, 'field')
    }
    gingko.ports.data.send(model)
    undoRedoMenuState(model.treePast, model.treeFuture)
    setTextarea(model, field)
  })
}

importFile = filepath => {
  fs.readFile(filepath, (err, data) => {
    if (err) throw err;
    setTitleFilename(filepath)
   
    var nextId = 1
    data = data.toString()
            .replace( /{(\s*)"content":/g
                    , s => {
                        return `{"id":"${nextId++}","content":`;
                      }
                    )
    var seed = JSON.parse(data)

    if (seed.length == 1) {
      var newRoot = 
          { id: "0"
          , content: seed[0].content
          , children: seed[0].children
          }
    } else {
      var newRoot = 
          { id: "0"
          , content: path.basename(filepath)
          , children: seed
          }
    }

    model =
      { tree: newRoot
      , treePast: []
      , treeFuture: []
      , viewState: 
          { active: "0"
          , activePast: []
          , activeFuture: []
          , descendants: []
          , editing: null
          , field: ""
          }
      , nextId: nextId + 1
      , saved: true
      }

    gingko.ports.data.send(model)
  })
}

newFile = function() {
  setTitleFilename(null)
  gingko.ports.data.send(null)
  undoRedoMenuState([],[])
  remote.getCurrentWindow().focus()
}

openDialog = function() { // TODO: add defaultPath
  dialog.showOpenDialog(
    null, 
    { title: "Open File..."
    , defaultPath: `${app.getPath('documents')}/../`
    , properties: ['openFile']
    , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }
    , function(e) {
        if(!!e) {
          attemptLoadFile(e[0])
        }
      }
 )
}

importDialog = function() {
  dialog.showOpenDialog(
    null, 
    { title: "Import File..."
    , defaultPath: `${app.getPath('documents')}/../`
    , properties: ['openFile']
    , filters:  [ {name: 'Gingko App JSON (*.json)', extensions: ['json']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }
    , function(e) {
        if(!!e) {
          importFile(e[0])
        }
      }
 )
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
      ipcRenderer.send('request-message')
      lastRequestTime = t

      if(isNaN(requestCount)) {
        requestCount = 1;
      } else {
        requestCount++
      }
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

jQuery(document).on('click', 'a[href^="http"]', function(ev) {
  ev.preventDefault()
  shell.openExternal(this.href)
})

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


/* === Menu state === */

undoRedoMenuState = (past, future) => {
  if (past.length === 0) {
    editSubMenu.items[0].enabled = false;
  } else {
    editSubMenu.items[0].enabled = true;
  }

  if (future.length === 0) {
    editSubMenu.items[1].enabled = false;
  } else {
    editSubMenu.items[1].enabled = true;
  }
}


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
