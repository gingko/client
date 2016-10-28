var jQuery = require('jquery')
var ipc = require('electron').ipcRenderer
var _ = require('underscore')
var autosize = require('textarea-autosize')
const path = require('path')
const dialog = require('electron').remote.dialog
const fs = require('fs')



/* === Initialization === */

var model = null
var currentFile = null
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
      break
    case 'save':
      model = msg[1]
      saveModel(model)
      break
  }
})


saveModel = function(model){
  if (currentFile) {
    fs.writeFile(currentFile, JSON.stringify(model, null, 2),function (err) { 
      if(err) { 
        dialog.showMessageBox({title: "Save Error", message: "Document wasn't saved."})
        console.log(err.message)
      }

      document.title = document.title.replace('*', '')
    })
  } else {
    saveModelAs(model)
  }
}

saveModelAs = function(model){
  dialog.showSaveDialog({title: 'Save As', defaultPath: `${__dirname}/..` }, function(e){
    currentFile = e
    document.title = `Gingko - ${path.basename(e)}`
    fs.writeFile(e, JSON.stringify(model, null, 2),function (err) { if(err) { console.log(err.message)}})
  })
}


/* === Messages To Elm === */

// From Main Process

ipc.on('save-and-close', (event, message) => {
  gingko.ports.externals.send(['save-and-close', ""])
})

ipc.on('file-read', (event, filename, message) => {
  var json = JSON.parse(message)
  document.title = `Gingko - ${path.basename(filename)}`
  startElm(json)
})

ipc.on('save-as-markdown', (event, message) => {
  gingko.ports.externals.send(['save-as-markdown', ""])
})

ipc.on('save-as-json', (event, message) => {
  gingko.ports.externals.send(['save-as-json', ""])
})


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
