var db = new PouchDB('elm-gingko-test-cards');
var viewState = JSON.parse(localStorage.getItem('elm-gingko-viewState'));
var commit = localStorage.getItem('elm-gingko-commit');
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
      , viewState: viewState ? viewState : {active: "0", editing: null, field: ""}
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
  gingko.ports.saveCurrentCommit.subscribe(saveCurrentCommit);
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
  commit["_id"] = commit["id"];
  delete commit["id"];
  commit["type"] = "commit";
  db.put(commit)
    .catch(function(err){console.log(err)});
}

saveCurrentCommit = function(currentCommit) {
  console.log(currentCommit);
  localStorage.setItem("elm-gingko-commit", currentCommit);
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


/* Keyboard shortcuts */
var shortcuts = [ 'mod+enter'
                , 'enter'
                , 'mod+j'
                , 'mod+k'
                , 'mod+l'
                , 'mod+s'
                , 'mod+z'
                , 'mod+r'
                ];

var needOverride= [ 'mod+j'
                  , 'mod+l'
                  , 'mod+s'  
                  , 'mod+r'
                  ];
                    
Mousetrap.bind(shortcuts, function(e, s) {
  gingko.ports.keyboard.send(s);

  if(needOverride.includes(s)) {
    return false;
  }
});



/* React rendering of Commits graph */
var commitsObject = [
  {
    "parents": [
      "82aa2102c8291f56f8dfefce1dce40d8a0dd686b",
      "175dfbbdbf8734069efaafced5a531dbf77c3a57"
    ],
    "sha": "5a7e04df76e21f9ba4a48098b6b26f19b51b99b1"
  },
  {
    "parents": [
      "90113cac59463df2e182e48444b8395658ebf840"
    ],
    "sha": "175dfbbdbf8734069efaafced5a531dbf77c3a57"
  },
  {
    "parents": [
      "82aa2102c8291f56f8dfefce1dce40d8a0dd686b"
    ],
    "sha": "90113cac59463df2e182e48444b8395658ebf840"
  },
  {
    "parents": [
      "395037182d9c34de2c26afc65b045ee875c93d25",
      "3f3d28b62f46f40959e31d54dbe21941801e91e3"
    ],
    "sha": "82aa2102c8291f56f8dfefce1dce40d8a0dd686b"
  },
  {
    "parents": [
      "6c7fa94319d3ab727b5606ad2bd0a732e65e7012"
    ],
    "sha": "3f3d28b62f46f40959e31d54dbe21941801e91e3"
  },
  {
    "parents": [
      "639f3e8d021af55953b2d9ee55bf83a0193c7797"
    ],
    "sha": "6c7fa94319d3ab727b5606ad2bd0a732e65e7012"
  },
  {
    "parents": [
      "f22fae5a4b3bcdcd8583f9cd25da8fbacad8fcf3"
    ],
    "sha": "639f3e8d021af55953b2d9ee55bf83a0193c7797"
  },
  {
    "parents": [
      "3b4cb078964d91ed9e9d422bd61087c552a928a4"
    ],
    "sha": "f22fae5a4b3bcdcd8583f9cd25da8fbacad8fcf3"
  },
  {
    "parents": [
      "f6b061794ccd2f9a46daefe082cae360dea85a41"
    ],
    "sha": "3b4cb078964d91ed9e9d422bd61087c552a928a4"
  },
  {
    "parents": [
      "2e00fd89a9c551e51d3f7a44044b4d29ed33bbd2"
    ],
    "sha": "f6b061794ccd2f9a46daefe082cae360dea85a41"
  },
  {
    "parents": [
      "395037182d9c34de2c26afc65b045ee875c93d25"
    ],
    "sha": "2e00fd89a9c551e51d3f7a44044b4d29ed33bbd2"
  },
  {
    "parents": [
      "4cde659b0b50e3ffe4ef73def1a077d47c241b44"
    ],
    "sha": "395037182d9c34de2c26afc65b045ee875c93d25"
  },
  {
    "parents": [
      "8e0b15259fbb7dd339b9aca646501b4203973874"
    ],
    "sha": "4cde659b0b50e3ffe4ef73def1a077d47c241b44"
  },
  {
    "parents": [
      "acaf8ad845d28fb9b4119bcfc1500c9ce1a903b5"
    ],
    "sha": "8e0b15259fbb7dd339b9aca646501b4203973874"
  },
  {
    "parents": [
      "4335d3ab62d8b5531508a27e622ad186d86bcdc7"
    ],
    "sha": "acaf8ad845d28fb9b4119bcfc1500c9ce1a903b5"
  },
  {
    "parents": [
      "cd10df3588ca6858af0201c28b58a8259289e3f1"
    ],
    "sha": "4335d3ab62d8b5531508a27e622ad186d86bcdc7"
  },
  {
    "parents": [
      "496f8a4e3aaa53630c573076a9d988e020cbf1ca"
    ],
    "sha": "cd10df3588ca6858af0201c28b58a8259289e3f1"
  },
  {
    "parents": [
      "ee9f4af79ff6fab50f7ebdf823f06dfdbc000b24"
    ],
    "sha": "496f8a4e3aaa53630c573076a9d988e020cbf1ca"
  },
  {
    "parents": [
      "4b247b9f19b0b345755aaae1914a280e66d09a0c"
    ],
    "sha": "ee9f4af79ff6fab50f7ebdf823f06dfdbc000b24"
  },
  {
    "parents": [
      "2cb54f702fa5f0ffd2435dbae7b05d52f8124c1e"
    ],
    "sha": "4b247b9f19b0b345755aaae1914a280e66d09a0c"
  },
  {
    "parents": [
      "69d9c1b99fa70108614a453a91c57c2945261dfa"
    ],
    "sha": "2cb54f702fa5f0ffd2435dbae7b05d52f8124c1e"
  },
  {
    "parents": [
      "503867bfcf9b5b615909225af2410e8225c8a6a4"
    ],
    "sha": "69d9c1b99fa70108614a453a91c57c2945261dfa"
  },
  {
    "parents": [
      "d62b312eb28f396ff67afecdd68ca9bcf4adf722"
    ],
    "sha": "503867bfcf9b5b615909225af2410e8225c8a6a4"
  },
  {
    "parents": [
      "071e7bde5b16832be2a2e48f1f5c91733644adf5"
    ],
    "sha": "d62b312eb28f396ff67afecdd68ca9bcf4adf722"
  },
  {
    "parents": [
      "e72dd3608da184c8760d011bd4320b902f5fdc0b"
    ],
    "sha": "071e7bde5b16832be2a2e48f1f5c91733644adf5"
  },
  {
    "parents": [
      "0e2a0bfb87ae21c79518a6c6dc6aa9aee0bf6dfb"
    ],
    "sha": "e72dd3608da184c8760d011bd4320b902f5fdc0b"
  },
  {
    "parents": [
      "e41d31976f002cee56a917dde2c1c75615e39d63"
    ],
    "sha": "0e2a0bfb87ae21c79518a6c6dc6aa9aee0bf6dfb"
  },
  {
    "parents": [
      "96cd2f670d5829716a9a98d543d063f4fe304026"
    ],
    "sha": "e41d31976f002cee56a917dde2c1c75615e39d63"
  },
  {
    "parents": [
      "7d9b3577930547e3549f630cf21160fdbcaea87e"
    ],
    "sha": "96cd2f670d5829716a9a98d543d063f4fe304026"
  },
  {
    "parents": [
      "876ba955da6788d60180261ec884b34fc73e574c"
    ],
    "sha": "7d9b3577930547e3549f630cf21160fdbcaea87e"
  },
  {
    "parents": [
      "cc74c787c3e6f174700fac0f146acf67fe4233f4"
    ],
    "sha": "876ba955da6788d60180261ec884b34fc73e574c"
  },
  {
    "parents": [
      "50a5dae8710e0d7cd5aaa9bd240ff2ac2135f463"
    ],
    "sha": "cc74c787c3e6f174700fac0f146acf67fe4233f4"
  },
  {
    "parents": [
      "ac7dcd4513d30a970d3576be4492f82815a5964b"
    ],
    "sha": "50a5dae8710e0d7cd5aaa9bd240ff2ac2135f463"
  },
  {
    "parents": [],
    "sha": "ac7dcd4513d30a970d3576be4492f82815a5964b"
  }
]

function handleGraphClick(sha) {
  commit = sha;
  render();
}

function render() {
  ReactDOM.render(
    React.createElement( CommitsGraph
      , { commits: commitsObject
        , onClick: handleGraphClick
        , selected: commit
        })
  , document.getElementById('graph')
  );
}

