const _ = require("lodash");
const { TweenMax } = require("gsap");

/* ==== Utility functions ===== */

function toHex(s) {
  // utf8 to latin1
  var s = unescape(encodeURIComponent(s));
  var h = "";
  for (var i = 0; i < s.length; i++) {
    h += s.charCodeAt(i).toString(16);
  }
  return h;
}

/* ===== DOM Manipulation ===== */

var setTextarea = (m, f) => {
  if (m.viewState.editing !== null && f !== null) {
    var textarea = document.getElementById("card-edit-" + m.viewState.editing);
    textarea.value = f;
  }
};

var scrollHorizontal = (colIdx, instant) => {
  lastColumnIdx = colIdx;
  _.delay(scrollHorizTo, 20, colIdx, instant);
};

var scrollColumns = (centerlineIds, instant) => {
  lastCenterline = centerlineIds;
  centerlineIds.map(function (c, i) {
    var centerIdx = Math.round(c.length / 2) - 1;
    _.delay(scrollTo, 20, c[centerIdx], i, instant);
  });
};

var scrollColumns2 = (scrollInfo) => {
  scrollInfo.columns.map(column => {
    let positionParam;
    switch (column.scrollData.position) {
      case "Center":
        positionParam = "center";
        break;

      case "After":
        positionParam = "bottom";
        break;

      case "Before":
        positionParam = "top";
        break;

      case "Between":
        positionParam = "bottom"
        break;
    }
    _.delay(scrollTo, 20, column.scrollData.target, column.columnIdx -1, scrollInfo.instant, positionParam);
  });
}

var scrollTo = function (cid, colIdx, instant, position) {
  var card = document.getElementById("card-" + cid.toString());
  var col = document.getElementsByClassName("column")[colIdx + 1];
  if (card == null) {
    console.log("scroll error: not found", cid);
    return;
  }
  var rect = card.getBoundingClientRect();
  let positionMultiplier = position === "top" ? 0 : position === "center" ? 0.5 : 1;

  TweenMax.to(col, instant ? 0 : 0.35, {
    scrollTop:
      col.scrollTop + (rect.top + rect.height * positionMultiplier - col.offsetHeight * 0.5),
    ease: Power2.easeInOut,
  });
};

var scrollHorizTo = function (colIdx, instant) {
  let scrollDuration = instant ? 0 : 0.5;
  var col = document.getElementsByClassName("column")[colIdx];
  var appEl = document.getElementById("document");
  if (col == null) {
    console.log("scroll horiz error: not found", colIdx);
    return;
  }
  var rect = col.getBoundingClientRect();
  if (rect.width >= appEl.offsetWidth) {
    TweenMax.to(appEl, scrollDuration, {
      scrollLeft: appEl.scrollLeft + rect.left,
      ease: Power2.easeInOut,
    });
  } else if (rect.left < 100) {
    TweenMax.to(appEl, scrollDuration, {
      scrollLeft: appEl.scrollLeft - 100 + rect.left,
      ease: Power2.easeInOut,
    });
  } else if (rect.right > appEl.offsetWidth - 100) {
    TweenMax.to(appEl, scrollDuration, {
      scrollLeft: appEl.scrollLeft + 100 + rect.right - appEl.offsetWidth,
      ease: Power2.easeInOut,
    });
  } else {
  }
};

/* ===== Shared variables ===== */

const errorAlert = (title, msg, err) => {
  return {
    title: title,
    message: msg,
    detail: err.message.split("\n")[0],
    type: "error",
    buttons: ["OK"],
  };
};

var shortcuts = [
  "shift+enter",
  "mod+enter",
  "enter",
  "esc",
  "mod+backspace",
  "mod+j",
  "mod+down",
  "mod+k",
  "mod+up",
  "mod+l",
  "mod+right",
  "mod+shift+j",
  "mod+shift+down",
  "mod+shift+k",
  "mod+shift+up",
  "h",
  "j",
  "k",
  "l",
  "left",
  "down",
  "up",
  "right",
  "alt+left",
  "alt+h",
  "alt+down",
  "alt+j",
  "alt+up",
  "alt+k",
  "alt+right",
  "alt+l",
  "alt+shift+up",
  "alt+shift+down",
  "alt+home",
  "alt+end",
  "[",
  "]",
  "mod+x",
  "mod+c",
  "mod+v",
  "mod+shift+v",
  "mod+z",
  "mod+r",
  "mod+b",
  "mod+i",
  "end",
  "home",
  "pageup",
  "pagedown",
  "/",
  "w",
];

var needOverride = [
  "mod+n",
  "mod+o",
  "mod+shift+s",
  "mod+h",
  "mod+j",
  "mod+k",
  "mod+l",
];

/* ===== CommonJS Module exports ===== */

module.exports = {
  scrollHorizontal: scrollHorizontal,
  scrollColumns: scrollColumns,
  scrollColumns2: scrollColumns2,
  errorAlert: errorAlert,
  shortcuts: shortcuts,
  needOverride: needOverride,
  toHex: toHex,
};
