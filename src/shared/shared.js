const _ = require('underscore')


/* ===== Database ===== */

function saveModel(db, model) {
  var data = nodesToRows(model.nodes)
  console.log('data',data)

  db.allDocs({
    include_docs: true
  }).then(function (result) {

    var dataDb = result.rows.map(function(r) {
      return r.doc
    })
    console.log('dataDb',dataDb)

    console.log('diff',_.difference(data, dataDb))


  }).catch(function (err) {
    console.log(err)
  })

  db.bulkDocs(data)
    .then(function (result) {
      console.log(result)
  }).catch(function(err) {
      console.log(err)
  })
}

function loadModel(db, callback) {
  db.allDocs({
    include_docs: true
  }).then(function (result) {
    callback(rowsToNodes(result.rows))
  }).catch(function (err) {
    console.log(err)
  })
}

function rowsToNodes(rows) {
  return rows.reduce(function(map, obj) {
    map[obj.doc._id] =  
      { content: obj.doc.content
      , children: obj.doc.children
      , rev: obj.doc._rev
      }
    return map
  }, {})
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

/* ===== CommonJS Module exports ===== */

module.exports = 
  { scrollHorizontal: scrollHorizontal
  , scrollColumns: scrollColumns
  , saveModel: saveModel
  , loadModel: loadModel
  }
