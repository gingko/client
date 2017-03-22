//var jQuery = require('jquery')
var _ = require('underscore')
//var autosize = require('textarea-autosize')
const url = require('url')
window.Elm = require('../elm.js')



/* === Global Variables === */

var gingko
var field = null
var editing = null
var blankAutosave = null
var currentSwap = null
var saved = true
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

var query = url.parse(location.toString(), true).query;

if((query.name && query.email) && !(!!email && !!name)) {
  name = query.name
  email = query.email
  localStorage.setItem('name', name)
  localStorage.setItem('email', email)
  window.Intercom('update', {email: email, name: name})
}

if (isNaN(firstRunTime)) {
  firstRunTime = Date.now()
  localStorage.setItem('firstRunTime', firstRunTime)
}
if (isTrial == null) {
  isTrial = true
  localStorage.setItem('isTrial', true)
}


/* === Initializing App === */

if(location.hash !== "") {
  var model;
  filepath = decodeURIComponent(location.hash.slice(1))  

  try {
    contents = fs.readFileSync(filepath)

    if(contents !== null) {
      model = JSON.parse(contents)
      if (model.field !== undefined) {
        field = model.field
        editing = model.viewState.editing
        model = _.omit(model, 'field')
      }
      setTitleFilename(filepath)
    }
  }
  catch (err) {
    console.log(err)
    dialog.showErrorBox("File load error.", err.message)
  }
  gingko =  Elm.Main.fullscreen(model)
} else {
  gingko =  Elm.Main.fullscreen(null)
}


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
  scrollHorizontal(actives[0])
  scrollColumns(actives[1])
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
  undoRedoMenuState([],[])
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

//jQuery(document).on('click', 'a[href^="http"]', function(ev) {
//  ev.preventDefault()
//  shell.openExternal(this.href)
//})

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
      if(editing == t.id.split('-')[2] && field !== null) {
        t.value = field
        t.focus()
      }
      t.oninput = editingInputHandler;
    })
    //jQuery(textareas).textareaAutoSize()
  }
});
 
var config = { childList: true, subtree: true };
 
observer.observe(document.body, config);
