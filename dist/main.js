var jQuery = require('jquery')
var ipc = require('electron').ipcRenderer
var _ = require('underscore')
var autosize = require('textarea-autosize')

var gingko = null;

ipc.on('file-read', (event, message) => {
  var json = JSON.parse(message)
  console.log(json)
  startElm(json)
})

startElm = function(init) {
  gingko = Elm.Main.fullscreen(init);
  gingko.ports.saveModel.subscribe(saveModel);
  gingko.ports.activateCards.subscribe(activateCards);
  gingko.ports.export.subscribe(exportTree);
}

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
  ipc.send('save', model)
}

saveObjects = function(objects, type) {
  data = objects.map(function(o){
    o["type"] = type
    return o
  })

  db.bulkDocs(data)
    .catch(function(err){console.log(err)})
}

saveCommit = function(commit) {
  var parentIndex = commits.map(function(c){return c._id}).indexOf(commit.parents[0]);
  if(parentIndex == 0) {
    commits.unshift(JSON.parse(JSON.stringify(commit)));
  } else {
    parentIndex = Math.max(parentIndex-1, 1);
    commits.splice(parentIndex,0, JSON.parse(JSON.stringify(commit)));
  }

  commit["type"] = "commit";
  db.put(commit)
    .catch(function(err){console.log(err)});
}


sortCommits = function(coms) {
  var remaining = coms.slice(0);
  var result = [];

  var findOne = function (haystack, arr) {
    return arr.some(function (v) {
        return haystack.indexOf(v) >= 0;
    });
  };
  
  result = remaining.filter(function(c){ return c.parents.length == 0 });
  remaining = remaining.filter(function(c){ return c.parents.length !== 0 });

  if(result.length == 0) {
    result = remaining.filter(function(c){
      var hasParent = commits.some(function(c1){return c.parents.includes(c1._id)})
      return !hasParent;
    });

    remaining = remaining.filter(function(c){
      var hasParent = commits.some(function(c1){return c.parents.includes(c1._id)})
      return hasParent;
    });

  }
  
  var x = coms.length + 10;

  while (remaining.length && x > 0) {
    if (x == 1) { alert('error sorting commits'); }

    // ids of all commits already in result
    var pids = result.map(function(c){ return c._id });

    // ids of commits in remaining with one of pids as its parent
    var toAdd = remaining
                .filter(function(c){
                  return findOne(c.parents, pids)
                })


    toAdd.map(function(t){
      result.unshift(t); 
      remaining = remaining.filter(function(r){ return r !== t });
    });

    x--;
  }

  return result;
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

// Commands with payloads
ipc.on('commit-changes', (event, message) => {
  gingko.ports.externals.send(['commit-changes', Date.now().toString()])
})

ipc.on('save-as-markdown', (event, message) => {
  gingko.ports.externals.send(['save-as-markdown', ""])
})

ipc.on('save-as-json', (event, message) => {
  gingko.ports.externals.send(['save-as-json', ""])
})

// Keyboard shortcuts
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

// create an observer instance
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
 
// configuration of the observer:
var config = { childList: true, subtree: true };
 
// pass in the target node, as well as the observer options
observer.observe(document.body, config);
