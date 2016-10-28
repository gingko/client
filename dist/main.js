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
    scrollTo(c[centerIdx], i)
  })
})


gingko.ports.message.subscribe(function(msg) {
  switch (msg[0]) {
    case 'save-temp':
      model = msg[1]
      document.title = 
        /\*/.test(document.title) ? document.title : document.title + "*"
      saved = false
      break
    case 'save':
      model = msg[1]
      saveModel(model, saveCallback)
      break
  }
})




/* === Messages From Main Process === */




/* === Handlers === */

window.onbeforeunload = function (e) {
  if(!saved) {
    var options = 
      { title: "test"
      , message: "Save changes before closing?"
      , buttons: ["Close Without Saving", "Cancel", "Save"]
      , defaultId: 2
      }
    var choice = dialog.showMessageBox(options)

    if (choice !== 0) {
      e.returnValue = false

      if (choice == 2) {
        attemptSave(model, () => app.exit(), (err) => console.log(err))
      }
    }
  }
}


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
    currentFile = e
    document.title = `Gingko - ${path.basename(e)}`
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




/* === Messages To Elm === */

// From This Renderer Process

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
  if (card == null) return;
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
