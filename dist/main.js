var jQuery = require('jquery')
var _ = require('underscore')
var autosize = require('textarea-autosize')
const fs = require('fs')
const path = require('path')
const ipc = require('electron').ipcRenderer
const remote = require('electron').remote
const app = remote.app
const dialog = remote.dialog



/* === Initialization === */

var model = null
var currentFile = null
var saved = true
var gingko =  Elm.Main.fullscreen(null)




/* === Elm Ports === */

gingko.ports.activateCards.subscribe(function(centerlineIds) {
  centerlineIds.map(function(c, i){
    var centerIdx = Math.round(c.length/2) - 1
    _.delay(scrollTo, 20, c[centerIdx], i)
  })
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
      saved = false
      break
  }
})




/* === Handlers === */

ipc.on('new', function(e) {
  if(!saved) {
    var options = 
      { title: "Save changes"
      , message: "Save changes before closing?"
      , buttons: ["Close Without Saving", "Cancel", "Save"]
      , defaultId: 2
      }
    var choice = dialog.showMessageBox(options)

    if (choice == 0) {
      newFile() 
    } else if (choice == 2) {
      attemptSave(model, () => newFile(), (err) => console.log(err))
    }
  } else {
    gingko.ports.data.send(null)
  }
})


ipc.on('load', function(e) {
  if(!saved) {
    var options = 
      { title: "Save changes"
      , message: "Save changes before closing?"
      , buttons: ["Close Without Saving", "Cancel", "Save"]
      , defaultId: 2
      }
    var choice = dialog.showMessageBox(options)

    if (choice == 0) {
      loadFile() 
    } else if (choice == 2) {
      attemptSave(model, () => loadFile(), (err) => console.log(err))
    }
  } else {
    loadFile()
  }
})


ipc.on('save', function(e) {
  saveModel(model, saveCallback)
})

ipc.on('save-as', function(e) {
  saveModelAs(model, saveCallback)
})


ipc.on('export-as-json', function(e) {
  var strip = function(tree) {
    return {"content": tree.content, "children": tree.children.map(strip)}
  }

  dialog.showSaveDialog({title: 'Export JSON', defaultPath: `${__dirname}/..` }, function(e){
    fs.writeFile(e, JSON.stringify([strip(model.tree)], null, 2), function(err){ 
      if(err) { 
        dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
        console.log(err.message)
      }
    })
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

  dialog.showSaveDialog({title: 'Export Markdown', defaultPath: `${__dirname}/..` }, function(e){
    fs.writeFile(e, flattenTree(model.tree, []).join("\n\n"), function(err){ 
      if(err) { 
        dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
        console.log(err.message)
      }
    })
  })
})

ipc.on('save-and-close', function (e) {
  attemptSave(model, () => app.exit(), (err) => console.log(err))
})


attemptSave = function(model, success, fail) {
  saveModel(model, function(err){
    if (err) { fail(err) } 
    success()
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
  dialog.showSaveDialog({title: 'Save As', defaultPath: `${__dirname}/..` }, function(e){
    setCurrentFile(e)
    fs.writeFile(e, JSON.stringify(model, null, 2), cb)
  })
}


saveCallback = function(err) {
  if(err) { 
    dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
    console.log(err.message)
  }

  document.title = document.title.replace('*', '')
  saved = true
}


setCurrentFile = function(filepath) {
  currentFile = filepath
  saved = true
  document.title = `Gingko - ${path.basename(filepath)}`
}




/* === Messages To Elm === */

newFile = function() {
  setCurrentFile('Untitled')
  gingko.ports.data.send(null)
  remote.getCurrentWindow().focus()
}


loadFile = function() {
  dialog.showOpenDialog(null, {title: "Open File...", defaultPath: `${__dirname}/..`, properties: ['openFile']}, function(e) {
    fs.readFile(e[0], (err, data) => {
      if (err) throw err;
      setCurrentFile(e[0])
      gingko.ports.data.send(JSON.parse(data))
    })
  })
}


var shortcuts = [ 'mod+enter'
                , 'enter'
                , 'esc'
                , 'mod+backspace'
                , 'mod+j'
                , 'mod+k'
                , 'mod+l'
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
                , 'mod+s'
                , 'mod+z'
                , 'mod+r'
                , 'mod+x' // debug command
                ];

var needOverride= [ 'mod+j'
                  , 'mod+l'
                  , 'mod+s'  
                  , 'mod+r'
                  ];
                    
Mousetrap.bind(shortcuts, function(e, s) {
  gingko.ports.externals.send(['keyboard', s]);

  if(needOverride.includes(s)) {
    return false;
  }
});




/* === DOM manipulation === */

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
