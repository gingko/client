var db = new PouchDB('elm-gingko-test-cards');
var ipc = require('electron').ipcRenderer

var viewState = JSON.parse(localStorage.getItem('elm-gingko-viewState'));
var commit = localStorage.getItem('elm-gingko-commit');
var commits = null;
var gingko = null;


db.allDocs({include_docs: true}).then(function(docs){
  contents = docs.rows
            .filter(function(row){
              return row.doc.type == "content" 
            })
            .map(function(row){
              row.doc["id"] = row.doc["_id"];
              delete row.doc["_id"];
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  nodes = docs.rows
            .filter(function(row){
              return row.doc.type == "node" 
            })
            .map(function(row){
              row.doc["id"] = row.doc["_id"];
              delete row.doc["_id"];
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  

  commits = docs.rows
            .filter(function(row){
              return row.doc.type == "commit" 
            })
            .map(function(row){
              row.doc["id"] = row.doc["_id"];
              delete row.doc["_id"];
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
              row.doc["id"] = row.doc["_id"];
              delete row.doc["_id"];
              delete row.doc["_rev"];
              delete row.doc["type"];
              return row.doc;
            });  
            
  
  var startingState = null;

  if (contents.length > 0 && nodes.length > 0 && commits.length > 0 ) {
    startingState =
      { objects :
        { contents: contents
        , nodes: nodes
        , commits : commits
        , operations : operations
        }
      , commit: commit ? commit : "bc5ff95381ff8577663e45455b14cd09d7e126c1"
      , viewState: viewState ? viewState : {active: "0", descendants: [], editing: null, field: ""}
      }
  }

  startElm(startingState);

}).catch(function(err){
  startElm(null);
});

startElm = function(init) {
  gingko = Elm.Main.fullscreen(init);
  gingko.ports.saveContents.subscribe(saveContents);
  gingko.ports.saveNodes.subscribe(saveNodes);
  gingko.ports.saveCommit.subscribe(saveCommit);
  gingko.ports.setCurrentCommit.subscribe(setCurrentCommit);
  gingko.ports.saveOp.subscribe(saveOp);
  gingko.ports.activateCard.subscribe(activateCard);
}

scrollTo = function(cid) {
  var card = document.getElementById('card-' + cid.toString());
  var col = card.closest('.column');
  if (card == null) return;
  var rect = card.getBoundingClientRect();

  TweenMax.to(col, 0.35,
    { scrollTop: col.scrollTop + ((rect.top + rect.height*0.5) - col.offsetHeight*0.5)
    , ease: Power2.easeInOut
    });
}


saveContents = function(contents) {
  data = contents.map(function(c){
    return  { _id: c.id
            , content: c.content
            , contentType : c.contentType
            , type : "content"
            }
  });
  db.bulkDocs(data)
    .catch(function(err){console.log(err)});
}

saveNodes = function(nodes) {
  data = nodes.map(function(c){
    return  { _id: c.id
            , contentId: c.contentId
            , childrenIds : c.childrenIds
            , type : "node"
            }
  });
  db.bulkDocs(data)
    .catch(function(err){console.log(err)});
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

  commit["_id"] = commit["id"];
  delete commit["id"];
  commit["type"] = "commit";
  db.put(commit)
    .catch(function(err){console.log(err)});
}

setCurrentCommit = function(currentCommit) {
  console.log('setCurrentCommit', currentCommit);
  localStorage.setItem("elm-gingko-commit", currentCommit);
  commit = currentCommit;
  render();
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

activateCard = function(uid) {
  scrollTo(uid.toString());
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
                , 'mod+j'
                , 'mod+k'
                , 'mod+l'
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
