const _ = require('underscore')
const PouchDB = require('pouchdb')
var db = new PouchDB('my_database')
var remoteCouch = 'http://localhost:5984/kittens'


/* ===== Database ===== */

db.sync(remoteCouch, {live: true}, (err) => console.log(err))

function saveModel(model) {
  var data = 
    { _id: new Date().toISOString()
    , model: model
    }

  db.put(data, function callback(err, result) {
    if (!err) {
      console.log("Saving to PouchDB success: ", result)
    } else {
      console.log('failure: ' + err)
    }
  })
}

function loadModel(callback) {
  db.allDocs({include_docs: true, descending: true}, function(err, doc) {
    callback(doc.rows[0]) 
  })
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
