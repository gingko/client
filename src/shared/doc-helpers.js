const _ = require('lodash')
const {TweenMax} = require('gsap')


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

const errorAlert = (title, msg, err) => {
  return { title: title
    , message: msg
    , detail: err.message.split("\n")[0]
    , type: "error"
    , buttons: ["OK"]
    }
}



var shortcuts = [ "mod+shift+enter"
                , "mod+enter"
                , "enter"
                , "esc"
                , "mod+backspace"
                , "mod+j"
                , "mod+k"
                , "mod+l"
                , "mod+down"
                , "mod+up"
                , "mod+right"
                , "h"
                , "j"
                , "k"
                , "l"
                , "left"
                , "down"
                , "up"
                , "right"
                , "alt+left"
                , "alt+h"
                , "alt+down"
                , "alt+j"
                , "alt+up"
                , "alt+k"
                , "alt+right"
                , "alt+l"
                , "alt+shift+up"
                , "alt+shift+down"
                , "alt+home"
                , "alt+end"
                , "["
                , "]"
                , "mod+x"
                , "mod+c"
                , "mod+v"
                , "mod+shift+v"
                , "mod+z"
                , "mod+r"
                , "mod+b"
                , "mod+i"
                , "end"
                , "home"
                , "pageup"
                , "pagedown"
                , "/"
                , "w"
                ];

var needOverride= [ 'mod+n'
                  , 'mod+o'
                  , 'mod+shift+s'
                  ];

/* ===== CommonJS Module exports ===== */

module.exports =
  { scrollHorizontal: scrollHorizontal
  , scrollColumns: scrollColumns
  , errorAlert : errorAlert
  , shortcuts: shortcuts
  , needOverride: needOverride
  };
