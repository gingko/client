var db = new PouchDB('elm-gingko-test-cards');
var ipc = require('electron').ipcRenderer
var _ = require('underscore')

var gingko = null;

// in-memory data, for quick deduping
var contents = []
var nodes = []
var commits = []
var operations = []
var tree = null
var commit = null
var floating = []
var viewState = null

db.allDocs({include_docs: true}).then(function(docs){
  contents = docs.rows
            .filter(function(row){
              return row.doc.type == "content" 
            })
            .map(function(row){
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  nodes = docs.rows
            .filter(function(row){
              return row.doc.type == "node" 
            })
            .map(function(row){
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  commits = docs.rows
            .filter(function(row){
              return row.doc.type == "commit" 
            })
            .map(function(row){
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  commits = sortCommits(commits);

  operations = docs.rows
            .filter(function(row){
              return row.doc.type == "operation" 
            })
            .map(function(row){
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  tree = JSON.parse(localStorage.getItem('gingko-tree'))
  commit = JSON.parse(localStorage.getItem('gingko-commit'))
  floating = JSON.parse(localStorage.getItem('gingko-floating'))
  viewState = JSON.parse(localStorage.getItem('gingko-viewState'))
            
  
  var startingState =
    { contents: contents
    , nodes: nodes
    , commits: commits
    , operations: operations
    , tree: tree
    , floating: floating
    , commit: commit
    , viewState: viewState  
    }

  startElm(startingState);

}).catch(function(err){
  startElm(null);
});

startElm = function(init) {
  gingko = Elm.Main.fullscreen(init);
  gingko.ports.saveModel.subscribe(saveModel);
  gingko.ports.activateCards.subscribe(activateCards);
}

scrollTo = function(cid, colIdx) {
  var card = document.getElementById('card-' + cid.toString());
  var col = document.getElementsByClassName('column')[colIdx]
  if (card == null) return;
  var rect = card.getBoundingClientRect();

  TweenMax.to(col, 0.35,
    { scrollTop: col.scrollTop + ((rect.top + rect.height*0.5) - col.offsetHeight*0.5)
    , ease: Power2.easeInOut
    });
}

saveModel = function(model) {
  console.log('model json from Elm', model)
  saveObjects(_.difference(model.contents, contents), "content")
  saveObjects(_.difference(model.nodes, nodes), "node")
  saveObjects(_.difference(model.commits, commits), "commit")
  saveObjects(_.difference(model.operations, operations), "operation")

  localStorage.setItem('gingko-tree', JSON.stringify(model.tree))
  localStorage.setItem('gingko-commit', JSON.stringify(model.commit))
  localStorage.setItem('gingko-floating', JSON.stringify(model.floating))
  localStorage.setItem('gingko-viewState', JSON.stringify(model.viewState))
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
  var parentIndex = commits.map(function(c){return c.id}).indexOf(commit.parents[0]);
  if(parentIndex == 0) {
    commits.unshift(JSON.parse(JSON.stringify(commit)));
  } else {
    parentIndex = Math.max(parentIndex-1, 1);
    commits.splice(parentIndex,0, JSON.parse(JSON.stringify(commit)));
  }
  render();

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
      var hasParent = commits.some(function(c1){return c.parents.includes(c1.id)})
      return !hasParent;
    });

    remaining = remaining.filter(function(c){
      var hasParent = commits.some(function(c1){return c.parents.includes(c1.id)})
      return hasParent;
    });

  }
  
  var x = coms.length + 10;

  while (remaining.length && x > 0) {
    if (x == 1) { alert('error sorting commits'); }

    // ids of all commits already in result
    var pids = result.map(function(c){ return c.id });

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

saveOp = function(op) {
  var prev = JSON.parse(localStorage.getItem("elm-test-ops"));
  prev = prev ? prev : [];
  if (op.opType == "Commit") {
    localStorage.setItem("elm-test-ops", JSON.stringify([]));
  } else {
    localStorage.setItem("elm-test-ops", JSON.stringify(prev.concat(op)));
  }
}

activateCards = function(centerlineIds) {
  centerlineIds.map(function(c, i){
    var centerIdx = Math.round(c.length/2) - 1
    scrollTo(c[centerIdx], i)
  })
}

// Commands with payloads
ipc.on('commit-changes', (event, message) => {
  gingko.ports.externals.send(['commit-changes', Date.now().toString()])
})

function handleGraphClick(sha) {
  commit = sha;
  gingko.ports.externals.send(['checkout-commit', sha]);
  render();
}

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




function render() {
  ReactDOM.render(
    React.createElement( CommitsGraph
      , { commits: commits.map(function(c){ return { "sha": c.id, "parents": c.parents }})
        , onClick: handleGraphClick
        , selected: commit
        })
  , document.getElementById('graph')
  );
}
