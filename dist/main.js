var jQuery = require('jquery')
var ipc = require('electron').ipcRenderer
var _ = require('underscore')
var autosize = require('textarea-autosize')
const path = require('path')

var gingko = null;


/* === Elm Initialization === */

startElm = function(init) {
  gingko = Elm.Main.fullscreen(init);
  gingko.ports.saveModel.subscribe(saveModel);
  gingko.ports.activateCards.subscribe(activateCards);
  gingko.ports.export.subscribe(exportTree);
  gingko.ports.message.subscribe(elmMessage);
}




/* === Messages To Elm === */

// From Main Process

ipc.on('save-and-close', (event, message) => {
  gingko.ports.externals.send(['save-and-close', ""])
})

ipc.on('file-read', (event, filename, message) => {
  var json = JSON.parse(message)
  document.title = path.basename(filename)
  startElm(json)
})

ipc.on('test-msg', (event, message) => {
  console.log('test-msg log:',message)
})

ipc.on('commit-changes', (event, message) => {
  gingko.ports.externals.send(['commit-changes', Date.now().toString()])
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




/* === Messages From Elm === */

elmMessage = function(msg) {
  switch (msg[0]) {
    case 'save':
      ipc.send(msg[0], msg[1])
      break
    case 'save-and-close':
      ipc.send(msg[0], msg[1])
      break
  }
}




/* === Event Handlers === */

scrollTo = function(cid, colIdx) {
  var card = document.getElementById('card-' + cid.toString());
  var col = document.getElementsByClassName('column')[colIdx+1]
  if (card == null) return;
  var rect = card.getBoundingClientRect();

  TweenMax.to(col, 0.35,
    { scrollTop: col.scrollTop + ((rect.top + rect.height*0.5) - col.offsetHeight*0.5)
    , ease: Power2.easeInOut
    });
}


saveModel = function(model) {
  ipc.send('save-as', model)
}


activateCards = function(centerlineIds) {
  centerlineIds.map(function(c, i){
    var centerIdx = Math.round(c.length/2) - 1
    scrollTo(c[centerIdx], i)
  })
}


exportTree = function(arg) {
  if (typeof arg == "string") {
    ipc.send('save-as-markdown', arg)
  } else if (typeof arg == "object") {
    ipc.send('save-as-json', arg)
  }
}




/* === JS-only Events === */

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
