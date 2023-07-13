const _ = require("lodash");
const { gsap } = require("gsap");

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
let toElm;

const defineCustomTextarea = (toElmFn) => {
  if (!toElm) {
    toElm = toElmFn;
  }

  customElements.define('gw-textarea', class extends HTMLElement {
    constructor() {
      super();
      this.textarea_ = document.createElement('textarea');
      this.textarea_.addEventListener('input', this._onInput.bind(this));
      this.textarea_.addEventListener('keyup', this._selectionHandler.bind(this));
      this.textarea_.addEventListener('click', this._selectionHandler.bind(this));
      this.textarea_.addEventListener('focus', this._focusHandler.bind(this));
      this.textarea_.addEventListener('blur', this._blurHandler.bind(this));
    }
    set isFullscreen(value) {
      this._isFullscreen = value;
    }
    get isFullscreen() {
      return this._isFullscreen;
    }

    connectedCallback() {
      this.textarea_.value = this.getAttribute('start-value');
      this.textarea_.setAttribute('id', 'card-edit-' + this.getAttribute('card-id'));
      this.textarea_.setAttribute('card-id', this.getAttribute('card-id'));
      this.textarea_.setAttribute('class', this.getAttribute('class'));
      this.textarea_.setAttribute('dir', this.getAttribute('dir'));
      this.textarea_.setAttribute('data-private', this.getAttribute('data-private'));
      this.textarea_.setAttribute('data-gramm', this.getAttribute('data-gramm'));
      this.appendChild(this.textarea_);
      if (!this.isFullscreen) {
        this.textarea_.focus();
      }
      this.offset_ = this.textarea_.offsetHeight - this.textarea_.clientHeight;
      this._resize();
    }

    disconnectedCallback() {
      this.textarea_.removeEventListener('input', this._onInput.bind(this));
      this.textarea_.removeEventListener('keyup', this._selectionHandler.bind(this));
      this.textarea_.removeEventListener('click', this._selectionHandler.bind(this));
      this.textarea_.removeEventListener('focus', this._focusHandler.bind(this));
      this.textarea_.removeEventListener('blur', this._blurHandler.bind(this));
      updateFillets();
    }

    _onInput(e) {
      toElm(e.target.value, "docMsgs", "FieldChanged");
      this._resize();
    }

    _resize() {
      this.textarea_.style.height = 'auto';
      this.textarea_.style.height = this.textarea_.scrollHeight + this.offset_ + 'px';
    }

    _selectionHandler(e) {
      selectionHandler(e)
    }

    _focusHandler(e) {
      if (this.isFullscreen) {
        const cardId = this.textarea_.getAttribute('card-id');
        const cardContent = this.textarea_.value;
        scrollFullscreen(cardId);
        toElm([cardId, cardContent], "docMsgs", "FullscreenCardFocused");
      }
    }

    _blurHandler(e) {
      if (!this.isFullscreen) {
        editBlurHandler(e)
      }
    }
  });
}

const getObserver = (toElmFn) => {
  return;
  return new MutationObserver(function (mutations) {
    const isTextarea = function (node) {
      return node.nodeName == "TEXTAREA" && node.className == "edit mousetrap";
    };

    let textareas = [];

    mutations.map((m) => {
      [].slice.call(m.addedNodes).map((n) => {
        if (isTextarea(n)) {
          textareas.push(n);
        } else {
          if (n.querySelectorAll) {
            let tareas = [].slice.call(n.querySelectorAll("textarea.edit"));
            textareas = textareas.concat(tareas);
          }
        }
      });

      [].slice.call(m.removedNodes).map((n) => {
        if ("getElementsByClassName" in n && n.getElementsByClassName("edit mousetrap").length != 0) {
          updateFillets();
        }
      })
    });

    if (textareas.length !== 0) {
      textareas.map((t) => {
        t.onkeyup = selectionHandler
        t.onclick = selectionHandler
        t.onfocus = selectionHandler
      })
      needOverride.push("left", "right");
      if (Object.prototype.hasOwnProperty.call(toElm, 'toMain')) {
        toElm.toMain('edit-mode-changed', true)
      }
      if (document.getElementById("app-fullscreen") === null) {
        document.addEventListener('click', editBlurHandler)
      }
    } else {
      needOverride = _.without(needOverride, "left", "right");
      if (Object.prototype.hasOwnProperty.call(toElm, 'toMain')) {
        toElm.toMain('edit-mode-changed', false)
      }
      document.removeEventListener('click', editBlurHandler);
    }
  });
}

const selectionHandler = function () {
  if (document.activeElement.nodeName == "TEXTAREA") {
    let {
      selectionStart,
      selectionEnd,
      selectionDirection,
    } = document.activeElement;
    let length = document.activeElement.value.length;
    let [before, after] = [
      document.activeElement.value.substring(0, selectionStart),
      document.activeElement.value.substring(selectionStart),
    ];
    let cursorPosition = "other";

    if (length == 0) {
      cursorPosition = "empty";
    } else if (selectionStart == 0 && selectionEnd == 0) {
      cursorPosition = "start";
    } else if (selectionStart == length && selectionEnd == length) {
      cursorPosition = "end";
    } else if (selectionStart == 0 && selectionDirection == "backward") {
      cursorPosition = "start";
    } else if (selectionEnd == length && selectionDirection == "forward") {
      cursorPosition = "end";
    }

    toElm(
      {
        selected: selectionStart !== selectionEnd,
        position: cursorPosition,
        text: [before, after],
      },
      "docMsgs",
      "TextCursor"
    );
  }
};

const editBlurHandler = (ev) => {
  if (process.env.NODE_ENV === 'development') {
    console.log("editBlurHandler", ev)
  }
  let targetClasses = ev.target.classList;
  const parentClasses = ev.target.parentElement.classList;
  const grandparentClasses = ev.target.parentElement.parentElement.classList;

  const isSaveButton = ev.target.nodeName == "DIV" && targetClasses.contains("card-btn") && targetClasses.contains("save");
  const isFullscreenButton = ev.target.nodeName == "DIV" && targetClasses.contains("fullscreen-card-btn");
  const isFullscreenSvg = ev.target.nodeName == "svg" && parentClasses.contains("fullscreen-card-btn");
  const isFullscrerenPath = ev.target.nodeName == "path" && grandparentClasses.contains("fullscreen-card-btn");

  if (isSaveButton || isFullscreenButton || isFullscreenSvg || isFullscrerenPath || isEditTextarea(ev.target)) {
    return;
  } else {
    toElm(null, "docMsgs", "ClickedOutsideCard");
  }
};

const isEditTextarea = (node) => {
  return node.nodeName == "TEXTAREA" && node.classList.contains("edit") && node.classList.contains("mousetrap");
}

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
  var col = document.getElementById("app-fullscreen");
  if (card == null) {
    console.log("scroll error: not found", cid);
    return;
  }
  var rect = card.getBoundingClientRect();

  const newScrollTop = col.scrollTop + (rect.top + rect.height * 0.5 - col.offsetHeight * 0.5);
  console.log("scrollFullscreenTo", newScrollTop);

  gsap.to(col,  0.25, {
    scrollTop: newScrollTop,
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
    gsap.to(col, 0.25, {
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
    gsap.to(appEl, 0.3, {
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
    let bottomLeftDelta = Math.min(Math.max((filletData[colIdx].bottom - filletData[colIdx-1].bottom)*0.5, -16), 16);
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
        let bottomRightDelta = Math.min(Math.max((filletData[colIdx].bottom - filletData[colIdx+1].bottom)*0.5, -16), 16);

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

const updateFillets = (cols) => {
  let columns = cols || Array.from(document.getElementsByClassName("column"));
  let filletData = getFilletData(columns);
  columns.map((c,i) => {
    setColumnFillets(c,i, filletData);
  })
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

/* ===== Shared fromElm cases ===== */

var casesShared = (elmData, params) => {
  return {
    ScrollCards: () => {
      scrollColumns(elmData);
      scrollHorizontal(elmData.columnIdx, elmData.instant);
      params.lastActivesScrolled = elmData;
      params.lastColumnScrolled = elmData.columnIdx;
      if (params.localStore.isReady()) {
        params.localStore.set('last-actives', elmData.lastActives);
      }

      let columns = Array.from(document.getElementsByClassName("column"));

      if (columns.length == 0) {
        setTimeout(() => {
          updateFillets();
        }, 20);
      }

      columns.map((c, i) => {
        c.addEventListener('scroll', () => {
          if(!params.ticking) {
            params.ticking = true;
            window.requestAnimationFrame(() => {
              updateFillets(columns);
              params.ticking = false;
            })
          }
        })
      })

      window.requestAnimationFrame(() => {
        updateFillets(columns);
      });
    },

    CopyCurrentSubtree: () => {
      navigator.clipboard.writeText(JSON.stringify(elmData))
      const addFlashClass = function () {
        const activeCard = document.querySelectorAll('.card.active')
        const activeDescendants = document.querySelectorAll('.group.active-descendant')
        activeCard.forEach((c) => c.classList.add('flash'))
        activeDescendants.forEach((c) => c.classList.add('flash'))
      }

      const removeFlashClass = function () {
        const activeCard = document.querySelectorAll('.card.active')
        const activeDescendants = document.querySelectorAll('.group.active-descendant')
        activeCard.forEach((c) => c.classList.remove('flash'))
        activeDescendants.forEach((c) => c.classList.remove('flash'))
      }

      addFlashClass()
      setTimeout(removeFlashClass, 200)
    },

    TextSurround: () => {
      const id = elmData[0]
      const surroundString = elmData[1]
      const tarea = document.getElementById('card-edit-' + id)
      const card = document.getElementById('card-' + id)

      if (tarea === null) {
        console.log('Textarea not found for TextSurround command.')
      } else {
        const start = tarea.selectionStart
        const end = tarea.selectionEnd
        if (start !== end) {
          const text = tarea.value.slice(start, end)
          const modifiedText = surroundString + text + surroundString
          const newValue = tarea.value.substring(0, start) + modifiedText + tarea.value.substring(end)
          tarea.value = newValue
          const cursorPos = start + modifiedText.length
          tarea.setSelectionRange(cursorPos, cursorPos)
          params.DIRTY = true
          toElm(newValue, 'docMsgs', 'FieldChanged')

          if (card !== null) {
            card.dataset.clonedContent = newValue
          }
        }
      }
    },

    SetCursorPosition: () => {
      let pos = elmData[0];
      requestAnimationFrame(() => document.activeElement.setSelectionRange(pos, pos));
    },

    SetTextareaClone: () => {
      let id = elmData[0];
      let card = document.getElementById("card-" + id);
      if (card === null) {
        console.error("Card not found for autogrowing textarea");
      } else {
        card.dataset.clonedContent = elmData[1];
      }
    },

    ConfirmCancelCard: () => {
      const tarea = document.getElementById('card-edit-' + elmData[0])

      if (tarea === null) {
        console.log('tarea not found')
      } else {
        if (tarea.value === elmData[1] || window.confirm(elmData[2])) {
          toElm(null, 'docMsgs', 'CancelCardConfirmed')
        }
      }
    },

    ConsoleLogRequested: () => console.error(elmData),
  }
}

/* ===== CommonJS Module exports ===== */

module.exports = {
  defineCustomTextarea: defineCustomTextarea,
  getObserver: getObserver,
  scrollHorizontal: scrollHorizontal,
  scrollColumns: scrollColumns,
  scrollFullscreen: scrollFullscreen,
  updateFillets: updateFillets,
  errorAlert: errorAlert,
  shortcuts: shortcuts,
  needOverride: needOverride,
  toHex: toHex,
  casesShared: casesShared
};
