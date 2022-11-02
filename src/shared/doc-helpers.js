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
  scrollHorizTo(colIdx, instant, 0)
};

var scrollColumns = (scrollInfo) => {
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
    scrollTo(column.scrollData.target, column.columnIdx, scrollInfo.instant, positionParam, 0)
  });
}

var scrollFullscreen = function (cid) {
  scrollFullscreenTo(cid)
}

var scrollFullscreenTo = function (cid) {
  var card = document.getElementById("card-" + cid.toString());
  var col = document.getElementById("fullscreen-main");
  if (card == null) {
    console.log("scroll error: not found", cid);
    return;
  }
  var rect = card.getBoundingClientRect();

  TweenMax.to(col,  0.25, {
    scrollTop:
      col.scrollTop + (rect.top + rect.height * 0.5 - col.offsetHeight * 0.5),
    ease: "power2.easeInOut",
  });
}

var scrollTo = function (cid, colIdx, instant, position, errorCount) {
  var card = document.getElementById("card-" + cid.toString());
  var col = document.getElementsByClassName("column")[colIdx - 1];
  let doc = document.getElementById("document");
  if (card == null || doc == null) {
    console.log("scroll error: not found", cid, errorCount);
    window.requestAnimationFrame(()=>{
      if (errorCount <= 3) {
        errorCount++;
        scrollTo(cid, colIdx, instant, position, errorCount);
      }
    })
    return;
  }
  let docRect = doc.getBoundingClientRect();
  var rect = card.getBoundingClientRect();
  let positionMultiplier = position === "top" ? 0 : position === "center" ? 0.5 : 1;

  // For cards larger than the viewing area, scroll to the top of the card, not its middle
  let adjustedHeight = (rect.height > docRect.height - 51) ? docRect.height * 0.5 + 51 : rect.height;

  let newScrollTop = col.scrollTop + (rect.top + adjustedHeight * positionMultiplier - (docRect.top+docRect.bottom)*0.5);

  if (instant) {
    col.scrollTop = newScrollTop;
  } else {
    TweenMax.to(col, 0.25, {
      scrollTop: newScrollTop,
      ease: "power2.easeInOut",
    });
  }
};

var scrollHorizTo = function (colIdx, instant, errorCount) {
  var col = document.getElementsByClassName("column")[colIdx - 1];
  var appEl = document.getElementById("document");

  let scrollDoneCallback = () => {};
  if (!instant) {
    /* Remove CSS scroll-snap behavior, otherwise won't animate */
    appEl.classList.add("scroll-animation");
    scrollDoneCallback = () => { appEl.classList.remove("scroll-animation") }
  }

  if (col == null) {
    console.log("scroll horiz error: not found", colIdx, errorCount);
    window.requestAnimationFrame(()=>{
      if (errorCount <= 3) {
        errorCount++;
        scrollHorizTo(colIdx, instant, errorCount);
      }
    })
    return;
  }
  if (instant) {
    appEl.scrollLeft = col.offsetLeft + 0.5  * (col.offsetWidth - appEl.offsetWidth);
  } else {
    TweenMax.to(appEl, 0.3, {
      scrollLeft: col.offsetLeft + 0.5  * (col.offsetWidth - appEl.offsetWidth),
      ease: "power2.easeInOut",
      onComplete: scrollDoneCallback
    });
  }
};

/* ===== Fillet Calculations ===== */

// List (Html) -> List (Maybe {top: Int, bottom: Int})
const getFilletData = (columnNodes) => {
  let getColumnTopBottomInfo = (col) => {
    let activeCards = col.getElementsByClassName("card active");
    let activeGroups = col.getElementsByClassName("group active-descendant");
    if (activeCards.length > 0) {
      let activeRect = activeCards[0].getBoundingClientRect();
      return {top: activeRect.top, bottom: activeRect.bottom};
    } else if (activeGroups.length > 0) {
      let firstActiveGroupRect = activeGroups[0].getBoundingClientRect();
      let lastActiveGroupRect = activeGroups[activeGroups.length - 1].getBoundingClientRect();
      return {top: firstActiveGroupRect.top, bottom: lastActiveGroupRect.bottom};
    } else {
      return null;
    }
  };

  return columnNodes.map(getColumnTopBottomInfo)
};

const setColumnFillets = (currColumn, colIdx, filletData) => {
  if(filletData[colIdx-1] == null && filletData[colIdx] != null && filletData[colIdx + 1] != null) {
    // Active Card
    let activeCard = currColumn.getElementsByClassName("card active")[0];
    if (activeCard) {
      let topDelta = Math.min(Math.max((filletData[colIdx].top - filletData[colIdx+1].top)*0.5, -16), 16);
      let bottomDelta = Math.min(Math.max((filletData[colIdx].bottom - filletData[colIdx+1].bottom)*0.5, -16), 16);
      let [filletTop, filletBottom] = activeCard.getElementsByClassName("fillet");

      if (filletTop && filletBottom) {
        filletTop.style.display = "block";
        filletBottom.style.display = "block";
        setTop(topDelta, filletTop);
        setBottom(bottomDelta, filletBottom);
      }
    }

  } else if(filletData[colIdx] != null && filletData[colIdx-1] != null) {
    // Active Descendant
    let topLeftDelta = Math.min(Math.max((filletData[colIdx].top - filletData[colIdx-1].top)*0.5, -16), 16);
    let bottomLeftDelta = Math.min(Math.max(filletData[colIdx].bottom - filletData[colIdx-1].bottom, -16), 16);
    let allTopLeftFillets = currColumn.getElementsByClassName("fillet top-left");
    let allBottomLeftFillets= currColumn.getElementsByClassName("fillet bottom-left");
    let filletTopLeft = allTopLeftFillets[0];
    let filletBottomLeft = allBottomLeftFillets[allBottomLeftFillets.length - 1];

    if(filletTopLeft && filletBottomLeft) {
      filletTopLeft.style.display = "block";
      filletBottomLeft.style.display = "block";
      Array.from(allTopLeftFillets).map((f,i) => {if (i!=0) {f.style.display = "none";}})
      Array.from(allBottomLeftFillets).map((f,i,a) => {if (i != a.length - 1) {f.style.display = "none";}})

      setTop(topLeftDelta, filletTopLeft);
      setBottom(bottomLeftDelta, filletBottomLeft);
    }

    // Active Descendant Right (Nested If)
    let filletTopRight = currColumn.getElementsByClassName("fillet top-right")[0];
    let allBottomRightFillets= currColumn.getElementsByClassName("fillet bottom-right");
    let filletBottomRight = allBottomRightFillets[allBottomRightFillets.length - 1];
    if (filletTopRight && filletBottomRight) {
      if (filletData[colIdx + 1] != null) {
        filletTopRight.style.display = "block";
        filletBottomRight.style.display = "block";
        let topRightDelta = Math.min(Math.max((filletData[colIdx].top - filletData[colIdx+1].top)*0.5, -16), 16);
        let bottomRightDelta = Math.min(Math.max(filletData[colIdx].bottom - filletData[colIdx+1].bottom, -16), 16);

        setTop(topRightDelta, filletTopRight);
        setBottom(bottomRightDelta, filletBottomRight);
      } else {
        filletTopRight.style.display = "none";
        filletBottomRight.style.display = "none";
      }
    }
  }
}

const setTop = (delta, el) => {
  if (delta > 0) {
    el.style.height = delta + "px";
    el.style.top = "-" + delta + "px";
    el.classList.remove("flipped");
  } else {
    el.style.height = -delta + "px";
    el.style.top = 0;
    el.classList.add("flipped");
  }
}

const setBottom = (delta, el) => {
  if (delta > 0) {
    el.style.height = delta + "px";
    el.style.bottom = 0;
    el.classList.add("flipped");
  } else {
    el.style.height = -delta + "px";
    el.style.bottom = delta + "px";
    el.classList.remove("flipped");
  }
}

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
  "mod+s",
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
  "alt+pageup",
  "alt+pagedown",
  "alt+0",
  "alt+1",
  "alt+2",
  "alt+3",
  "alt+4",
  "alt+5",
  "alt+6",
  "[",
  "]",
  "mod+o",
  "mod+x",
  "mod+c",
  "mod+v",
  "mod+shift+v",
  "mod+z",
  "mod+shift+z",
  "mod+r",
  "mod+b",
  "mod+i",
  "end",
  "home",
  "pageup",
  "pagedown",
  "/",
  "w",
  "?",
];

var needOverride = [
  "mod+s",
  "mod+n",
  "mod+o",
  "mod+h",
  "mod+j",
  "mod+k",
  "mod+l",
  "mod+b",
  "mod+i",
  "alt+0",
  "alt+1",
  "alt+2",
  "alt+3",
  "alt+4",
  "alt+5",
  "alt+6",
];

if (window.navigator.platform.toUpperCase().indexOf('MAC') < 0 ) {
  needOverride.push("alt+left");
}

/* ===== CommonJS Module exports ===== */

module.exports = {
  scrollHorizontal: scrollHorizontal,
  scrollColumns: scrollColumns,
  scrollFullscreen: scrollFullscreen,
  getFilletData: getFilletData,
  setColumnFillets: setColumnFillets,
  errorAlert: errorAlert,
  shortcuts: shortcuts,
  needOverride: needOverride,
  toHex: toHex,
};
