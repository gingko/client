var jQuery = require('jquery')
var _ = require('underscore')
var autosize = require('textarea-autosize')
const fs = require('fs')
const path = require('path')
const {ipcRenderer, webFrame, remote, shell} = require('electron')
const machineIdSync = require('electron-machine-id').machineIdSync
const app = remote.app
const dialog = remote.dialog
const Menu = remote.Menu



/* === Initialization === */

var model = {"tree":{"id":"0","content":"","children":[]},"treePast":[],"treeFuture":[],"viewState":{"active":"0","activePast":[],"activeFuture":[],"descendants":[],"editing":"0"},"nextId":1}
var field = null
var currentFile = null
var currentSwap = null
var blankAutosave = null
var saved = true

/* === Config loading === */

var firstRunTime = Number.parseInt(localStorage.getItem('firstRunTime'))
var lastRequestTime = Number.parseInt(localStorage.getItem('lastRequestTime'))
var isTrial = JSON.parse(localStorage.getItem('isTrial'))
var saveCount = Number.parseInt(JSON.parse(localStorage.getItem('saveCount')))
var requestCount = Number.parseInt(JSON.parse(localStorage.getItem('requestCount')))
var email = localStorage.getItem('email')
var name = localStorage.getItem('name')
var machineId = localStorage.getItem('machineId')

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


/* ====== */

var editSubMenu = Menu.getApplicationMenu().items[1].submenu;

setCurrentFile = function(filepath) {
  currentFile = filepath
  if (filepath && filepath.endsWith('.gko')) {
    currentSwap = filepath.replace('.gko', '.gko.swp') 
  } else { 
    currentSwap = null 
  }
  document.title =
    filepath ? `Gingko - ${path.basename(filepath)}` : "Gingko - Untitled"
}

setSaved = bool => {
  saved = bool;
  ipcRenderer.send('saved', bool)
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

if(location.hash !== "") {
  filepath = decodeURIComponent(location.hash.slice(1))  

  try {
    contents = fs.readFileSync(filepath)

    if(contents !== null) {
      model = JSON.parse(contents)
      if (model.field !== undefined) {
        field = model.field
        model = _.omit(model, 'field')
      }
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
      field = null
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
    field = null
    gingko.ports.updateSuccess.send([id, tarea.value])
  }
})




/* === Handlers === */

ipcRenderer.on('open-file', function(e) {
  console.log(e)
})


ipcRenderer.on('new', function(e) {
  saveConfirmAndThen(newFile)
})


ipcRenderer.on('open', function(e) {
  saveConfirmAndThen(openDialog)
})

ipcRenderer.on('import', function(e) {
  saveConfirmAndThen(importDialog)
})

ipcRenderer.on('save', function(e) {
  saveModel(model, saveCallback)
})

ipcRenderer.on('save-as', function(e) {
  saveModelAs(model, saveCallback)
})

ipcRenderer.on('clear-swap', function (e) {
  clearSwap()
})

ipcRenderer.on('save-and-close', function (e) {
  attemptSave(model, () => app.exit(), (err) => console.log(err))
})


ipcRenderer.on('export-as-json', function(e) {
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

ipcRenderer.on('export-as-markdown', function(e) {
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
    , defaultPath: currentFile ? `${app.getPath('documents')}/../${currentFile.replace('.gko','')}.txt` : `${app.getPath('documents')}/../Untitled.txt`
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
  if(email && name && window.Intercom) {
    window.Intercom('show')
  } else {
    ipcRenderer.send('ask-for-email')
  }
})

ipcRenderer.on('id-info', (e, msg) => {
  name = msg[0]
  email = msg[1]
  localStorage.setItem('name', name)
  localStorage.setItem('email', email)
  window.Intercom('update', {email: email, name: name})
  window.Intercom('show')
})

ipcRenderer.on('serial-success', e => {
  isTrial = false
  window.Intercom('update', { "unlocked": true })
  localStorage.setItem('isTrial', false)
})


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

jQuery(document).on('click', 'a[href^="http"]', function(ev) {
  ev.preventDefault()
  shell.openExternal(this.href)
})

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

withField = model => {
  if (field !== null) {
    return _.extend(model, {'field': field})
  } else {
    return model
  }
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

editingInputHandler = function(ev) {
  if (saved) {
    setSaved(false)
  }
  field = ev.target.value
}

saveModel = function(model, cb){
  var toSave = withField(model)
  if (currentFile) {
    fs.writeFile(currentFile, JSON.stringify(toSave, null, 2), cb)
  } else {
    saveModelAs(toSave, cb)
  }
}


saveModelAs = function(model, cb){
  var toSave = withField(model)
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
      fs.writeFile(e, JSON.stringify(toSave, null, 2), cb)
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
  undoRedoMenuState([],[])
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

var setTextarea = (m, f) => {
  if(m.viewState.editing !== null && f !== null) {
    var textarea = document.getElementById('card-edit-'+m.viewState.editing)
    textarea.value = f
  }
}

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
      if(model !== null && model.viewState.editing == t.id.split('-')[2] && field !== null) {
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

window.onload = function() {
  if (email && name) {
    window.Intercom("boot",
      { app_id: "g1zzjpc3"
      , user_id: machineId
      , email: email
      , name: name
      , created_at: Math.round(firstRunTime/1000)
      , "gingko_version": app.getVersion()
      }
    );
  } else {
    window.Intercom("boot",
      { app_id: "g1zzjpc3"
      , user_id: machineId
      , created_at: Math.round(firstRunTime/1000)
      , "gingko_version": app.getVersion()
      }
    );
  }
}
