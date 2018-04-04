const _ = require('lodash')
const {TweenMax} = require('gsap')


/* ===== Database Saving ===== */

function saveModel(db, model) {
  var data = nodesToRows(model.nodes)

  db.allDocs({
    include_docs: true
  }).then(function (result) {

    var dataDb = result.rows.map(function(r) {
      return r.doc
    })

    var deltas =
      getDeltas(dataDb, data)

    console.log(data, dataDb, deltas)

    db.bulkDocs(deltas)
      .then(function (result) {
        console.log('saved', result)
      }).catch(function(err) {
          console.log(err)
      })

  }).catch(function (err) {
    console.log(err)
  })
}

function getDeltas(oldData, newData) {
  var deltas = []

  newData.map(function (nd) {
    if (oldData.map(od => od._id).includes(nd._id)) {
      var od = _.find(oldData, od => od._id == nd._id);
      if (od._id == nd._id && !_.isEqual(od, nd)) {
        deltas.push([_.find(oldData, od => od._id == nd._id), nd])
      }
    }
    else {
      deltas.push([null, nd])
    }
  })

  return sortDeltas(deltas)
}

function sortDeltas(deltas) {
  var removals = []
  var insertions = []
  var others = []

  var isRemoval = function(d) {
    return d[0] !== null && _.differenceWith(d[0].children, d[1].children, _.isEqual).length !== 0
  }

  var isInsertion = function(d) {
    return d[0] !== null && _.differenceWith(d[1].children, d[0].children, _.isEqual).length !== 0
  }

  deltas.map(d => {
    if (isRemoval(d)) {
      console.log('delta:removal', d)
      removals.push(d[1])
    } else if (isInsertion(d)) {
      console.log('delta:insertion', d)
      insertions.push(d[1])
    } else {
      console.log('delta:other', d)
      others.push(d[1])
    }
  })

  var sorted = insertions.concat(removals).concat(others)
  console.log('delta:sorted', sorted)

  return sorted
}

function nodesToRows(nodes) {
  var rows = Object.keys(nodes).map(function(key) {
    return  { "_id": key
            , "_rev": nodes[key].rev
            , "content": nodes[key].content
            , "children": nodes[key].children
            }
  })

  return rows
}


/* ===== Database Loading ===== */

function loadModel(db, callback, error) {
  db.allDocs({
    include_docs: true
  }).then(function (result) {
    callback(result.rows)
  }).catch(function (err) {
    error(err)
  })
}

function rowsToNodes(rows) {
  return rows.reduce(function(map, obj) {
    map[obj.doc._id] =
      { content: obj.doc.content
      , children: obj.doc.children
      , rev: obj.doc._rev
      , deleted: obj.doc._deleted ? true : false
      }
    return map
  }, {})
}


function onChange(change) {
  console.log('change', change)
  var db = this.db
  if (change.doc._conflicts) {
    db.get(change.id, {
      open_revs: change.doc._conflicts
    })
    .then(function(responses) {
      console.log('conflict responses', responses)
      var docs = responses
        .filter(function(response){
          return 'ok' in response
        })
        .map(function(response) {
          return response.ok
        })
        .concat(change.doc)
      gingko.ports.conflicts.send(docs)
    })
  }
  else {
    gingko.ports.change.send(change.doc)
  }
}


function resolver(a, b) {
  console.log('resolver called', a, b)
  var m = _.clone(a)

  if (a.content !== b.content) {
    m.content = a.content + "\n=====CONFLICT=====\n" + b.content
  }

  if (!_.isEqual(a.children, b.children)) {
    m.children = _.unionWith(a.children, b.children, _.isEqual)
  }

  m.deleted = a.deleted || b.deleted

  return m
}


/* ===== DOM Manipulation ===== */

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
  var col = document.getElementsByClassName('column')[colIdx]
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
  } else {
  }
}


/* ===== Shared variables ===== */

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
                , 'alt+h'
                , 'alt+down'
                , 'alt+j'
                , 'alt+up'
                , 'alt+k'
                , 'alt+right'
                , 'alt+l'
                , 'alt+shift+up'
                , 'alt+shift+down'
                , 'alt+home'
                , 'alt+end'
                , '['
                , ']'
                , 'mod+z'
                , 'mod+r'
                , 'mod+n'
                , 'mod+s'
                , 'mod+o'
                , 'mod+b'
                , 'mod+i'
                , 'mod+x' // debug command
                ];

var needOverride= [ 'mod+j'
                  , 'mod+k'
                  , 'mod+l'
                  , 'mod+n'
                  , 'mod+s'
                  , 'mod+o'
                  , 'mod+r'
                  , 'alt+left'
                  , 'alt+right'
                  , 'alt+home'
                  ];

/* ===== CommonJS Module exports ===== */

module.exports =
  { scrollHorizontal: scrollHorizontal
  , scrollColumns: scrollColumns
  , saveModel: saveModel
  , loadModel: loadModel
  , onChange: onChange
  , resolver: resolver
  , shortcuts: shortcuts
  , needOverride: needOverride
  }
