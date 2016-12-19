var jQuery = require('jquery')
var _ = require('underscore')
var autosize = require('textarea-autosize')
const fs = require('fs')
const path = require('path')
const ipc = require('electron').ipcRenderer
var webFrame = require('electron').webFrame
const remote = require('electron').remote
const app = remote.app
const dialog = remote.dialog
const Menu = remote.Menu



/* === Initialization === */

var model = null
var currentFile = null
var currentSwap = null
var blankAutosave = null
var saved = true

setCurrentFile = function(filepath) {
  currentFile = filepath
  if (filepath && filepath.endsWith('.gko')) {
    currentSwap = filepath.replace('.gko', '.gko.swp') 
  } else { 
    currentSwap = null 
  }
  setSaved(true)
  document.title =
    filepath ? `Gingko - ${path.basename(filepath)}` : "Gingko - Untitled"
}

setSaved = bool => {
  saved = bool;
  ipc.send('saved', bool)
}

if(location.hash !== "") {
  filepath = decodeURIComponent(location.hash.slice(1))  

  try {
    contents = fs.readFileSync(filepath)

    if(contents !== null) {
      model = JSON.parse(contents)
      setCurrentFile(filepath)
    }
  }
  catch (err) {
    console.log(err)
    dialog.showErrorBox("File load error.", err.message)
  }
}
var gingko =  Elm.Main.fullscreen(model)
var lastCenterline = null
var lastColumnIdx = null




/* === Elm Ports === */

gingko.ports.activateCards.subscribe(actives => {
  scrollHorizontal(actives[0])
  scrollColumns(actives[1])
})

gingko.ports.message.subscribe(function(msg) {
  switch (msg[0]) {
    case 'save':
      model = msg[1]
      saveModel(model, saveCallback)
      break
    case 'save-temp':
      model = msg[1]
      document.title = 
        /\*/.test(document.title) ? document.title : document.title + "*"
      setSaved(false)
      autosave(model)
      break
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

gingko.ports.attemptUpdate.subscribe(id => {
  var tarea = document.getElementById('card-edit-'+id)

  if (tarea === null) {
    gingko.ports.updateError.send('Textarea with id '+id+' not found.')
  } else {
    gingko.ports.updateSuccess.send([id, tarea.value])
  }
})




/* === Handlers === */

ipc.on('open-file', function(e) {
  console.log(e)
})


ipc.on('new', function(e) {
  saveConfirmAndThen(newFile)
})


ipc.on('open', function(e) {
  saveConfirmAndThen(openDialog)
})

ipc.on('import', function(e) {
  saveConfirmAndThen(importDialog)
})

ipc.on('save', function(e) {
  saveModel(model, saveCallback)
})

ipc.on('save-as', function(e) {
  saveModelAs(model, saveCallback)
})

ipc.on('clear-swap', function (e) {
  clearSwap()
})

ipc.on('save-and-close', function (e) {
  attemptSave(model, () => app.exit(), (err) => console.log(err))
})


ipc.on('export-as-json', function(e) {
  var strip = function(tree) {
    return {"content": tree.content, "children": tree.children.map(strip)}
  }
  
  var options =
    { title: 'Export JSON'
    , defaultPath: currentFile ? `${app.getPath('documents')}/../${currentFile.replace('.gko','')}.json` : `${app.getPath('documents')}/../Untitled.json`
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
})

ipc.on('export-as-markdown', function(e) {
  var flattenTree = function(tree, strings) {
    if (tree.children.length == 0) {
      return strings.concat([tree.content])
    } else {
      return strings.concat([tree.content], _.flatten(tree.children.map(function(c){return flattenTree(c,[])})))
    }
  }

  var options =
    { title: 'Export Markdown (txt)'
    , defaultPath: currentFile ? `${app.getPath('documents')}/../${currentFile.replace('.gko','')}.txt` : `${app.getPath('documents')}/../Untitled.txt`
    , filters:  [ {name: 'Text Files', extensions: ['txt']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }

  dialog.showSaveDialog(options, function(e){
    if(!!e) {
      fs.writeFile(e, flattenTree(model.tree, []).join("\n\n"), function(err){ 
        if(err) { 
          dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
          console.log(err.message)
        }
      })
    }
  })
})


ipc.on('undo', function (e) {
  gingko.ports.externals.send(['keyboard','mod+z'])
})
ipc.on('redo', function (e) {
  gingko.ports.externals.send(['keyboard','mod+r'])
})


ipc.on('zoomin', e => {
  webFrame.setZoomLevel(webFrame.getZoomLevel() + 1)
})
ipc.on('zoomout', e => {
  webFrame.setZoomLevel(webFrame.getZoomLevel() - 1)
})
ipc.on('resetzoom', e => {
  webFrame.setZoomLevel(0)
})


saveConfirmAndThen = onSuccess => {
  if(!saved) {
    var options = 
      { title: "Save changes"
      , message: "Save changes before closing?"
      , buttons: ["Close Without Saving", "Cancel", "Save"]
      , defaultId: 2
      }
    var choice = dialog.showMessageBox(options)

    if (choice == 0) {
      onSuccess() 
    } else if (choice == 2) {
      attemptSave(model, () => onSuccess(), (err) => console.log(err))
    }
  } else {
    onSuccess()
  }
}

document.ondragover = document.ondrop = (ev) => {
  ev.preventDefault()
}

document.body.ondrop = (ev) => {
  saveConfirmAndThen(attemptLoadFile(ev.dataTransfer.files[0].path))
  ev.preventDefault()
}

window.onresize = () => {
  if (lastCenterline) { scrollColumns(lastCenterline) }
  if (lastColumnIdx) { scrollHorizontal(lastColumnIdx) }
}

attemptSave = function(model, success, fail) {
  saveModel(model, function(err){
    if (err) { fail(err) } 
    success()
  })
}


autosave = function(model) {
  if (currentFile) {
    currentSwap =
        currentFile.replace('.gko','.gko.swp')

  } else {
    blankAutosave =
      blankAutosave
        ? blankAutosave
        : Date.now()

    currentSwap =
      `${app.getPath('documents')}/Untitled-${blankAutosave}.gko.swp`

    localStorage.setItem('autosave', currentSwap) // TODO: warn when this exists.
  }

  fs.writeFile(currentSwap, JSON.stringify(model, null, 2), function(err){
    if(err) {
      dialog.showErrorBox("Autosave error.", err.message)
    } 
  })
}

saveModel = function(model, cb){
  if (currentFile) {
    fs.writeFile(currentFile, JSON.stringify(model, null, 2), cb)
  } else {
    saveModelAs(model, cb)
  }
}


saveModelAs = function(model, cb){
  var options =
    { title: 'Save As'
    , defaultPath: currentFile ? `${app.getPath('documents')}/../${currentFile.replace('.gko','')}` : `${app.getPath('documents')}/../Untitled.gko`
    , filters:  [ {name: 'Gingko Files (*.gko)', extensions: ['gko']}
                , {name: 'All Files', extensions: ['*']}
                ]
    }

  dialog.showSaveDialog(options, function(e){
    if(!!e){
      setCurrentFile(e)
      fs.writeFile(e, JSON.stringify(model, null, 2), cb)
    }
  })
}

clearSwap = function(filepath) {
  var file = filepath ? filepath : currentSwap
  fs.unlinkSync(file)
}

saveCallback = function(err) {
  if(err) { 
    dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
    console.log(err.message)
  }

  document.title = document.title.replace('*', '')
  setSaved(true)
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
    setCurrentFile(setpath ? setpath : filepath)
    model = JSON.parse(data)
    gingko.ports.data.send(model)
  })
}


importFile = filepath => {
  fs.readFile(filepath, (err, data) => {
    if (err) throw err;
    setCurrentFile(filepath)
   
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


/* === Messages To Elm === */

newFile = function() {
  setCurrentFile(null)
  gingko.ports.data.send(null)
  remote.getCurrentWindow().focus()
}


openDialog = function() {
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
  editSubMenu = Menu.getApplicationMenu().items[1].submenu;


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

var scrollHorizontal = colIdx => {
  lastColumnIdx = colIdx
  _.delay(scrollHorizTo, 20, colIdx)
}

var scrollColumns = centerlineIds => {
  lastCenterline = centerlineIds
  centerlineIds.map(function(c, i){
    var centerIdx = Math.round(c.length/2) - 1
    _.delay(scrollTo, 20, c[centerIdx], i)
  })
}

var scrollTo = function(cid, colIdx) {
  var card = document.getElementById('card-' + cid.toString());
  var col = document.getElementsByClassName('column')[colIdx+1]
  if (card == null) {
    console.log('scroll error: not found',cid)
    return;
  }
  var rect = card.getBoundingClientRect();

  TweenMax.to(col, 0.35,
    { scrollTop: col.scrollTop + ((rect.top + rect.height*0.5) - col.offsetHeight*0.5)
    , ease: Power2.easeInOut
    });
}

var scrollHorizTo = function(colIdx) {
  var col = document.getElementsByClassName('column')[colIdx+1]
  var appEl = document.getElementById('app');
  if (col == null) {
    console.log('scroll horiz error: not found', colIdx)
    return;
  }
  var rect = col.getBoundingClientRect();
  if (rect.width >= appEl.offsetWidth) {
    TweenMax.to(appEl, 0.50,
      { scrollLeft: appEl.scrollLeft + rect.left
      , ease: Power2.easeInOut
      });
  } else if (rect.left < 100) {
    TweenMax.to(appEl, 0.50,
      { scrollLeft: appEl.scrollLeft - 100 + rect.left
      , ease: Power2.easeInOut
      });
  } else if (rect.right > appEl.offsetWidth - 100) {
    TweenMax.to(appEl, 0.50,
      { scrollLeft: appEl.scrollLeft + 100 + rect.right - appEl.offsetWidth 
      , ease: Power2.easeInOut
      });
  }
}


var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    var nodesArray = [].slice.call(mutation.addedNodes)
    var textareas = nodesArray.filter(function(node){
      return (node.nodeName == "TEXTAREA" && node.className == "edit mousetrap")
    })

    if (textareas.length !== 0) {
      jQuery(textareas).textareaAutoSize()
    }
  });    
});
 
var config = { childList: true, subtree: true };
 
observer.observe(document.body, config);
