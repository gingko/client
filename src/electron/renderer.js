import { Elm } from "../elm/Electron/Electron";
const Mousetrap = require("mousetrap");
const helpers = require("../shared/doc-helpers");
const container = require("Container");

// Init Vars

window.elmMessages = [];
let lastActivesScrolled = null;
let lastColumnScrolled = null;
let ticking = false;
let DIRTY
let isUntitled
const localStore =
  {
    isReady: () => { return true },
    set: (key, value) => {
      window.electronAPI.localStoreSet(key, value)
    }
  }

// Init Elm
let gingkoElectron;

const init = async function (filePath, fileData, fileSettings, undoData, isUntitled) {
  let timestamp = Date.now();
  let globalData =
    { seed : timestamp
    , currentTime : timestamp
    , isMac : false
    };
  gingkoElectron = Elm.Electron.Electron.init({ flags: { filePath, fileData, fileSettings, undoData, globalData, isUntitled } })

  gingkoElectron.ports.infoForOutside.subscribe(function (elmdata) {
    fromElm(elmdata.tag, elmdata.data);
  });
};

window.electronAPI.fileReceived(async (event, d) => {
  if (d.fileData !== null) {
    DIRTY = false
  } else {
    DIRTY = true
  }
  isUntitled = d.isUntitled
  await init(d.filePath, d.fileData, d.fileSettings, d.undoData, d.isUntitled)
})

window.electronAPI.fileSaved((event, data) => {
  DIRTY = false
  isUntitled = data[2]
  toElm(data.slice(0, 2), 'docMsgs', 'SavedToFile')
})

window.electronAPI.commitDataResult((event, data) => {
  toElm(data, 'docMsgs', 'DataSaved')
})

window.electronAPI.clickedExport((event) => {
  toElm(null, 'docMsgs', 'ClickedExport')
})

window.electronAPI.clickedUndo((event) => {
  toElm('mod+z', 'docMsgs', 'Keyboard')
})

window.electronAPI.clickedCut((event) => {
  toElm('mod+x', 'docMsgs', 'Keyboard')
})

window.electronAPI.clickedCopy((event) => {
  toElm('mod+c', 'docMsgs', 'Keyboard')
})

window.electronAPI.clickedPaste((event) => {
  toElm('mod+v', 'docMsgs', 'Keyboard')
})

window.electronAPI.clickedPasteInto((event) => {
  toElm('mod+shift+v', 'docMsgs', 'Keyboard')
})

window.checkboxClicked = (cardId, number) => {
  toElm([cardId, number], 'docMsgs', 'CheckboxClicked')
}

window.onbeforeunload = (e) => {
  if (isUntitled) {
    window.electronAPI.maybeCloseWindow()
    e.returnValue = false
  } else if (DIRTY) {
    setTimeout(window.electronAPI.closeWindow, 200)
    e.returnValue = false
  }
}

/* === Elm / JS Interop === */

const fromElm = (msg, elmData) => {
  window.elmMessages.push({ tag: msg, data: elmData })
  window.elmMessages = window.elmMessages.slice(-10)

  const casesElectron = {
    SetDirty: () => {
      DIRTY = elmData
    },

    CommitData: () => {
      window.electronAPI.commitData(elmData)
    },

    DragStart: () => {
      const cardElement = elmData.target.parentElement
      const cardId = cardElement.id.replace(/^card-/, '')
      elmData.dataTransfer.setDragImage(cardElement, 0, 0)
      elmData.dataTransfer.setData('text', '')
      toElm(cardId, 'docMsgs', 'DragStarted')
    },

    ExportToFile: () => {
      window.electronAPI.exportFile(elmData)
    },

    SaveToFile: () => {
      window.electronAPI.saveFile(elmData)
    }
  }

  const params = { localStore, lastColumnScrolled, lastActivesScrolled, ticking, DIRTY }

  const cases = Object.assign(helpers.casesShared(elmData, params), casesElectron)

  try {
    cases[msg]();
  } catch (err) {
    console.error("Unexpected message from Elm : ", msg, elmData, err);
  }
}

function toElm (data, portName, tagName) {
  toElm.toMain = window.electronAPI.toMain
  const portExists = Object.prototype.hasOwnProperty.call(gingkoElectron.ports, portName)
  const tagGiven = typeof tagName === 'string'

  if (portExists) {
    var dataToSend

    if (tagGiven) {
      dataToSend = { tag: tagName, data: data }
    } else {
      dataToSend = data
    }
    gingkoElectron.ports[portName].send(dataToSend)
  } else {
    console.error('Unknown port', portName, data)
  }
}

/* === Keyboard === */

const desktopShortcuts = helpers.shortcuts
  .filter((x) => x !== 'mod+s' && x !== 'mod+o')

Mousetrap.bind(desktopShortcuts, function (e, s) {
  switch (s) {
    case "enter":
      if (document.activeElement.nodeName == "TEXTAREA") {
        return;
      } else {
        toElm("enter","docMsgs", "Keyboard");
      }
      break;

    case "mod+c":
      let exportPreview = document.getElementById("export-preview");
      if (exportPreview !== null) {
        return;
      } else {
        toElm("mod+c","docMsgs", "Keyboard");
      }
      break;

    case "mod+v":
    case "mod+shift+v":
      let elmTag = s === "mod+v" ? "Paste" : "PasteInto";

      navigator.clipboard.readText()
        .then(clipString => {
          try {
            let clipObj = JSON.parse(clipString);
            toElm(clipObj, "docMsgs", elmTag)
          } catch {
            toElm(clipString, "docMsgs", elmTag)
          }
        });
      break;

    case "alt+0":
    case "alt+1":
    case "alt+2":
    case "alt+3":
    case "alt+4":
    case "alt+5":
    case "alt+6":
      if (document.activeElement.nodeName == "TEXTAREA") {
        let num = Number(s[s.length - 1]);
        let currentText = document.activeElement.value;
        let newText = currentText.replace(/^(#{0,6}) ?(.*)/, num === 0 ? '$2' : '#'.repeat(num) + ' $2');
        document.activeElement.value = newText;
        DIRTY = true;
        toElm(newText, "docMsgs", "FieldChanged");

        let cardElementId = document.activeElement.id.replace(/^card-edit/, "card");
        let card = document.getElementById(cardElementId);
        if (card !== null) {
          card.dataset.clonedContent = newText;
        }
      }
      break;

    default:
      toElm(s, "docMsgs", "Keyboard");
  }

  if (helpers.needOverride.includes(s)) {
    return false;
  }
});

/* === DOM manipulation === */

const observer = helpers.getObserver(toElm)
const observerConfig = { childList: true, subtree: true }
observer.observe(document.body, observerConfig)
