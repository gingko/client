const {app, BrowserWindow, dialog, Menu, ipcMain, Notification} = require('electron')
import { autoUpdater } from "electron-updater"
const fs = require('fs-extra')
const path = require('path')
const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync
const Store = require('electron-store')
const _ = require("lodash");
import TurndownService from 'turndown'
const windowStateKeeper = require('electron-window-state')
const docList = require('./doc-list')
const fio = require('./file-io')
const getMenuTemplate = require("./menu");
const filenamify = require("filenamify");
const unhandled = require("electron-unhandled");
const isDev = require("electron-is-dev");
const GingkoError  = require("../shared/errors");
const errorAlert = require('../shared/doc-helpers').errorAlert
const { tr } = require("../shared/translation.js");
import SystemFonts from "system-font-families";
const systemFonts = new SystemFonts();


unhandled();

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let documentWindows = []
let docWindowMenuStates = {};
let docWindowData = {};
let winTrial, winSerial, winHome;
let _untitledDocs = 0
let _menuQuit = false
const hiddenStore = new Store({name: "kernel", encryptionKey: "79df64f73eab9bc0d7b448d2008d876e"})
const userStore = new Store({name: "config"})
let lang = userStore.get("language") || "en";


// Make Gingko single instance
const instanceLock = app.requestSingleInstanceLock();

if (!instanceLock) {
  app.quit();
} else {
  app.on("second-instance", (event, commandLine) => {
    if(commandLine[0].endsWith("electron") && typeof commandLine[2] == "string") {
      openDocument(commandLine[2]);
    } else if (winHome) {
      winHome.show();
    } else {
      createHomeWindow();
    }
  });
}


function createHomeWindow () {
  // Create the browser window.
  winHome = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: '#477085'
    , icon: `${__dirname}/static/leaf128.png`
    , webPreferences: { nodeIntegration: true }
    })

  // and load the html of the home window.
  var url = `file://${__dirname}/static/home.html`

  winHome.loadURL(url)
  winHome.hideMenu = true;

  updateMenu(null, false, winHome);

  winHome.on('closed', () => {
    winHome = null;
  })
}


function createDocumentWindow (swapFolderPath, originalPath, legacyFormat, jsonImportData) {
  let mainWindowState = windowStateKeeper(
    { defaultWidth: 1000
    , defaultHeight: 800
    , file: `window-state-${sha1(swapFolderPath)}.json`
    }
  )

  // Create the browser window.
  var win = new BrowserWindow(
    { width: mainWindowState.width
    , height: mainWindowState.height
    , x: mainWindowState.x || (documentWindows.length * 30)
    , y: mainWindowState.y || (documentWindows.length * 30)
    , show: false
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/static/leaf128.png`
    , webPreferences: { nodeIntegration: true }
    })

  documentWindows.push(win);
  docWindowMenuStates[win.id] =
    { "editMode": false
    , "columnNumber" : 1
    , "changed" : !!jsonImportData
    , "lastExportPath" : false
    , "isNew": !originalPath || jsonImportData
    , "recentDocumentList": docList.getRecentDocs()
    };

  mainWindowState.manage(win);

  // Add swapFolderPath variable to initialization data
  _.set(docWindowData, [win.id, "swapFolderPath"], swapFolderPath);

  // Add dbPath variable to initialization data
  if (legacyFormat) {
    _.set(docWindowData, [win.id, "originalPath"], legacyFormat.dbname);
    _.set(docWindowData, [win.id, "legacyFormat"], legacyFormat);
    _.set(docWindowData, [win.id, "dbPath"], swapFolderPath);
  } else {
    _.set(docWindowData, [win.id, "originalPath"], originalPath);
    _.set(docWindowData, [win.id, "dbPath"], path.join(swapFolderPath, "leveldb"));
  }

  let newTitle = "";

  if (originalPath) {
    newTitle = `${path.basename(originalPath)} - Gingko`;
  } else if (legacyFormat) {
    newTitle = `${legacyFormat.name} (Saved Internally) - Gingko`;
  } else {
    newTitle = "Untitled" + (_untitledDocs !== 0 ? ` (${_untitledDocs + 1})` : "") + " - Gingko";
    _untitledDocs += 1;
  }
  win.setTitle(newTitle);
  _.set(docWindowData, [win.id, "jsonImportData"], jsonImportData);

  var url = `file://${__dirname}/static/index.html`
  win.loadURL(url)

  win.on('ready-to-show', () => {
    win.show()
  })

  // Emitted when the window is closed.
  win.on('closed', () => {
    // Dereference the window object
    let index = documentWindows.indexOf(win)
    if (index !== -1) {
      documentWindows.splice(index, 1)
    }
  })
}


function getTrialActivations() {
  let activations = hiddenStore.get('activations', []).concat((new Date).toISOString())
  let uniqueActivations = Array.from(new Set(activations.map(d => d.substring(0,10))))
  if(activations !== uniqueActivations) {
    hiddenStore.set('activations', uniqueActivations)
  }
  return uniqueActivations
}


function validSerial(email, storedSerial) {
  // I've decided against complicated anti-piracy checks.
  // Instead, I want things as easy as possible for the user, while still being able to make a living.
  //
  // If you really can't afford Gingko, even after allowing for discounts, please get in touch with me first.
  let hash = sha1(email + "Super easy to crack!")
  let serial = [hash.substr(4,4), hash.substr(12,4), hash.substr(20,4), hash.substr(28,4)].join("-").toUpperCase()
  return storedSerial == serial
}

/* ==== Menu ==== */

function updateMenu (menuState, lang, win) {
  lang = lang || userStore.get("language") || "en";
  let originalPath = _.get(docWindowData, [win.id, "originalPath"]);
  let legacyFormat = _.get(docWindowData, [win.id, "legacyFormat"]);

  if (menuState) {
    menuState.recentDocumentList = docList.getRecentDocs();
  }

  let handlers =
    { new : newUntitled
    , open : openWithDialog
    , openRecent : (rdoc) => openDocumentOrFolder(rdoc.location, rdoc.name)
    , openHome : createHomeWindow
    , save : (item, focusedWindow) => {
        if (originalPath) {
          focusedWindow.webContents.send("menu-save");
        } else {
          focusedWindow.webContents.send("menu-save-as");
        }
      }
    , saveAs : async (item, focusedWindow) => {
        let saveAsReturn = legacyFormat ? await saveLegacyDocumentAs(focusedWindow) : await saveDocumentAs(focusedWindow);
        if (saveAsReturn && saveAsReturn.filepath) {
          focusedWindow.webContents.send("main:saved-file");
          _.set(docWindowData, [focusedWindow.id, "originalPath"], saveAsReturn.filepath);
          let focusedWinMenuState = docWindowMenuStates[focusedWindow.id];
          focusedWinMenuState.isNew = false;
          updateMenu(focusedWinMenuState, false, focusedWindow);
        }
      }
    , import : importDocument
    , quit : () => { _menuQuit = true; app.quit(); }
    , enterLicense : (item, focusedWindow) => createSerialWindow(focusedWindow, false)
    , fonts : (item, focusedWindow) => {
        const fonts = systemFonts.getFontsSync();
        focusedWindow.webContents.send("menu-font-selector", fonts);
      }
    , language : (lang, focusedWindow) => {
        focusedWindow.webContents.send("menu-language-select", lang);
        let focusedWinMenuState = docWindowMenuStates[focusedWindow.id];
          updateMenu(focusedWinMenuState, lang, focusedWindow);
      }
    };

  let menuTemplate = getMenuTemplate(menuState, handlers, lang, process.platform === "darwin");
  let menu = Menu.buildFromTemplate(menuTemplate);

  if (process.platform === "darwin") {
    Menu.setApplicationMenu(menu);
  } else if (typeof win !== "undefined") {
    if ("hideMenu" in win && win.hideMenu) {
      win.removeMenu();
    } else {
      win.setMenu(menu);
    }
  }
}

app.on("browser-window-focus", (ev, win) => {
  updateMenu(docWindowMenuStates[win.id], false, win);
});


ipcMain.on("doc:get-initial-data", (event, arg) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  event.returnValue = docWindowData[win.id];
});


ipcMain.on("doc:column-number-change", (event, cols) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  if (win) {
    let menuState = docWindowMenuStates[win.id];
    if (menuState.columnNumber !== cols) {
      _.set(menuState, "columnNumber", cols);
      updateMenu(menuState, false, win);
    }
  }
});


ipcMain.on("doc:edit-mode-toggle", (event, isEditing) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  if (win) {
    let menuState = docWindowMenuStates[win.id];
    if (menuState.editMode !== isEditing) {
      _.set(menuState, "editMode", isEditing);
      updateMenu(menuState, false, win);
    }
  }
});


ipcMain.on("doc:last-export-set", (event, lastPath) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  if (win) {
    let menuState = docWindowMenuStates[win.id];
    if (menuState.lastExportPath !== lastPath) {
      _.set(menuState, "lastExportPath", lastPath);
      updateMenu(menuState, false, win);
    }
  }
});


ipcMain.on("doc:save", async (event) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  if (win && _.get(docWindowData, [win.id, "originalPath"], false)) {
    let saveReturn = await saveDocument(win);
    if (saveReturn) {
      if(process.platform == "win32") {
        win.webContents.send("main:database-open");
      }
      setDocumentChanged(win, false);
      win.webContents.send("main:saved-file");
    }
  }
});


ipcMain.on("doc:save-as", async (event) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  let originalPath = _.get(docWindowData, [win.id, "originalPath"]);
  let legacyFormat = _.get(docWindowData, [win.id, "legacyFormat"]);

  if (win) {
    let saveAsReturn = legacyFormat ? await saveLegacyDocumentAs(win) : await saveDocumentAs(win);
    if (saveAsReturn && saveAsReturn.filepath) {
      win.webContents.send("main:saved-file");
      _.set(docWindowData, [win.id, "originalPath"], saveAsReturn.filepath);
      let focusedWinMenuState = docWindowMenuStates[win.id];
      focusedWinMenuState.isNew = false;
      updateMenu(focusedWinMenuState, false, win);
    }
  }
});


ipcMain.on("doc:set-changed", (event, changed) => {
  let win = BrowserWindow.fromWebContents(event.sender);
  if (win) {
    setDocumentChanged(win, changed);
  }
});


ipcMain.on("doc:language-changed", (event, data) => {
  lang = data;
});





/* ==== App Events ==== */

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on("ready", async () => {
  Menu.setApplicationMenu(null);

  // Auto Updater code
  autoUpdater.fullChangelog = true;

  autoUpdater.on("update-downloaded", info => {
    let turndownService = new TurndownService();
    if (Array.isArray(info.releaseNotes)){
      var releaseNotesText = info.releaseNotes.map(rn => {
        return (process.platform === "darwin" ? rn.note : turndownService.turndown(rn.note));
      }).join("\n").replace(/\*/g, "·");
    } else {
      releaseNotesText = process.platform === "darwin" ? info.releaseNotes : turndownService.turndown(info.releaseNotes).replace(/\*/g, "·");
    }

    let updateNotification = new Notification(
      { title: tr.updatePopup[lang](app.getName(), info.version)
      , body: tr.updatePopupBody[lang](releaseNotesText)
      });

    updateNotification.show();
  });

  if(!isDev) {
    autoUpdater.checkForUpdates().catch(err => {
      console.log("my catch", err);
    });
  }

  let email = userStore.get("email", "");
  let storedSerial = userStore.get("serial", "");

  let pathArgument = false;

  setTimeout(fio.truncateBackups, 10*60*1000);// delete excess backups 10 minutes in
  setInterval(fio.truncateBackups, 30*60*1000);// delete excess backups every 30 minutes

  // Development, with path of file to open passed as argument
  if(process.defaultApp && typeof process.argv[2] == "string") {
    if(await fs.pathExists(process.argv[2])) {
      pathArgument = process.argv[2];
    }
  // Production, with path of file to open passed as argument
  } else if (!process.defaultApp && typeof process.argv[1] == "string") {
    if(await fs.pathExists(process.argv[1])) {
      pathArgument = process.argv[1];
    }
  }

  if(pathArgument) {
    openDocument(pathArgument);
  } else {
    createHomeWindow();
    if(!validSerial(email, storedSerial)) {
      let activations = getTrialActivations();
      let limit = 30;
      let daysLeft = Math.max(limit - activations.length, 0);
      let trialDisplayDays = [25, 20, 15, 10, 8, 6, 5, 4, 3, 2, 1, 0];

      if(trialDisplayDays.includes(daysLeft)) {
        createTrialWindow(winHome, activations, limit);
      }
    }
  }
});


app.on("will-finish-launching", () => {
  app.on("open-file", (ev, path) => {
    ev.preventDefault();
    openDocument(path);
  });
});


async function newUntitled() {
  const swapRandomName = sha1(Date.now()+machineIdSync()).slice(20)
  const swapFolderPath = path.join(app.getPath('userData'), swapRandomName);
  await fio.newSwapFolder(swapFolderPath);
  createDocumentWindow(swapFolderPath, null);
}


async function openWithDialog() {
  let options = {title: "Open File...", defaultPath : app.getPath("documents") , properties: ["openFile"], filters: [ {name: "Gingko Files (*.gko)", extensions: ["gko"]} ]};

  var {filePaths} = await dialog.showOpenDialog(options);

  if(Array.isArray(filePaths) && !!filePaths[0]) {
    return await openDocument(filePaths[0]);
  }
}


async function openDocumentOrFolder(dbToLoad, docName) {
  if (/^[a-f0-9]{40}$/i.test(dbToLoad)) {
    const swapPath = path.join(app.getPath("userData"), dbToLoad);
    createDocumentWindow(swapPath, null, { "name": docName, "dbname" : dbToLoad });
    await addToRecentDocuments(dbToLoad);
    return true;
  } else if (path.isAbsolute(dbToLoad) && fs.pathExistsSync(dbToLoad)) {
    await openDocument(dbToLoad);
    return true;
  } else {
    removeFromRecentDocuments(dbToLoad);

    const documentNotFoundOptions =
      { title: "Document Not Found"
      , type: "warning"
      , message: `I'm looking in ${dbToLoad}.\nMaybe it was moved?\n\nI will remove it from the recent documents list...`
      , buttons: ["OK"]
      , defaultId: 0
      };

    await dialog.showMessageBox(documentNotFoundOptions);
    return false;
  }
}


async function openDocument(filepath) {
  try {
    let swapFolderPath = await fio.openFile(filepath);
    createDocumentWindow(swapFolderPath, filepath);
    await addToRecentDocuments(filepath);
    return filepath;
  } catch (err) {

    // If the swap folder already exists, it's either because the file is currently open,
    // Or because of a failed exit in the past.
    //
    // In the "already open" case, focus that window.
    // Otherwise, allow the user to choose:
    //   - "Recover" (load from swap)
    //   - "Discard" (load from file)
    if (err instanceof GingkoError && err.message.includes("Swap folder already exists")) {
      let existingDoc = documentWindows.filter(dW => _.get(docWindowData, [dW.id, "swapFolderPath"]) == err.data)[0];
      if (existingDoc) {
        existingDoc.focus();
      } else {
        const recoveryOptions =
          { title: tr.unsavedChangesFound[lang]
          , message: tr.unsavedChangesMsg[lang]
          , buttons: [tr.discard[lang], tr.cancel[lang], tr.recover[lang]]
          , defaultId: 2
          };
        const {response: choice} = await dialog.showMessageBox(recoveryOptions);

        switch (choice) {
          // Discard Unsaved Changes
          case 0:
            await fio.deleteSwapFolder(err.data);
            return openDocument(filepath);

          // Cancel
          case 1:
            return;

          // Recover
          case 2:
            createDocumentWindow(err.data, filepath);
            addToRecentDocuments(filepath);
            break;
        }
      }
    }
    console.log(err);
  }
}




async function importDocument() {
  var options =
      { title: "Open File..."
      , defaultPath: app.getPath("documents")
      , properties: ["openFile"]
      , filters:  [ {name: "Gingko JSON Files (*.json)", extensions: ["json"]}
                  , {name: "All Files", extensions: ["*"]}
                  ]
      };

  var {filePaths} = await dialog.showOpenDialog(winHome, options);

  if(filePaths) {
    let { swapFolderPath, jsonImportData } = await fio.dbFromFile(filePaths[0]);
    createDocumentWindow(swapFolderPath, null, null, jsonImportData);
    return true;
  }
}




/*
 * saveDocument : BrowserWindow -> Promise String Error
 *
 * Given a docWindow
 * Save that document's swap folder to its filepath
 * Return filepath if successful.
 *
 */

async function saveDocument (docWindow) {
  let swapFolderPath = _.get(docWindowData, [docWindow.id, "swapFolderPath"]);
  try {
    if (process.platform === "win32") { docWindow.webContents.send("main:database-close"); }
    const filepath = await fio.saveSwapFolder(swapFolderPath);
    return filepath;
  } catch (err) {
    throw err;
  }
}




/*
 * saveDocumentAs : BrowserWindow -> Promise { filepath: String, swapFolderPath : String } Error
 *
 * Given a docWindow
 * - Get a newFilepath with save dialog
 * - Call saveSwapFolderAs to copy swap folder and save it
 * - Set docWindow.swapFolderPath and docWindow's title
 * - Send "set-swap-folder" message to doc.js
 * Return { filepath, swapFolderPath } if successful.
 *
 */

async function saveDocumentAs (docWindow) {
  let saveOptions =
    { title: "Save As"
    , defaultPath: path.join(app.getPath("documents"), "Untitled.gko")
    , filters: [{ name: "Gingko Files (*.gko)", extensions: ["gko"] }]
    };

  let originalPath = _.get(docWindowData, [docWindow.id, "originalPath"]);
  let swapFolderPath = _.get(docWindowData, [docWindow.id, "swapFolderPath"]);

  const {filePath : newFilepath} = await dialog.showSaveDialog(docWindow, saveOptions);

  if (newFilepath === originalPath) {
    // Saving to same location.
    // Perform "Save" instead of "Save As"
    await saveDocument(docWindow);
    docWindow.setTitle(`${path.basename(newFilepath)} - Gingko`);
    return { "filepath" : newFilepath, "swapFolderPath" : swapFolderPath };
  }

  if (newFilepath) {
    try {
      if (process.platform === "win32") { docWindow.webContents.send("main:database-close"); }
      const newSwapFolderPath = await fio.saveSwapFolderAs(swapFolderPath, newFilepath);
      _.set(docWindowData, [docWindow.id, "swapFolderPath"], newSwapFolderPath);
      await addToRecentDocuments(newFilepath);
      docWindow.setTitle(`${path.basename(newFilepath)} - Gingko`);
      docWindow.webContents.send("main:set-swap-folder", [path.join(newSwapFolderPath,"leveldb"), newFilepath]);
      return { "filepath" : newFilepath, "swapFolderPath" : newSwapFolderPath };
    } catch (err) {
      throw err;
    }
  }
}




/*
 * saveLegacyDocumentAs : BrowserWindow -> Promise String Error
 *
 * Given a docWindow
 * - Get a newFilepath with save dialog
 * - Call saveLegacyFolderAs to copy swap folder and save it
 * - Set docWindow.swapFolderPath and docWindow's title
 * - Send "set-swap-folder" message to doc.js
 * Return new filepath if successful.
 *
 */

async function saveLegacyDocumentAs (docWindow) {
  let legacyFormat = _.get(docWindow, [docWindow.id, "legacyFormat"]);
  let swapFolderPath = _.get(docWindow, [docWindow.id, "swapFolderPath"]);

  let saveOptions =
    { title: "Save As"
    , defaultPath: path.join(app.getPath("documents"), filenamify(legacyFormat.name) + ".gko")
    , filters: [{ name: "Gingko Files (*.gko)", extensions: ["gko"] }]
    };

  const {filePath : newFilepath} = await dialog.showSaveDialog(docWindow, saveOptions);

  if (newFilepath) {
    try {
      if (process.platform === "win32") { docWindow.webContents.send("main:database-close"); }
      const newSwapFolderPath = await fio.saveLegacyFolderAs(swapFolderPath, legacyFormat.name, newFilepath);
      docList.setState(legacyFormat.dbname, "deprecated");
      _.set(docWindowData, [docWindow.id, "swapFolderPath"], newSwapFolderPath);
      addToRecentDocuments(newFilepath);
      docWindow.setTitle(`${path.basename(newFilepath)} - Gingko`);
      docWindow.webContents.send("main:set-swap-folder", [path.join(newSwapFolderPath,"leveldb"), newFilepath]);
      return { "filepath" : newFilepath, "swapFolderPath" : newSwapFolderPath };
    } catch (err) {
      throw err;
    }
  } else {
    return false;
  }
}




/*
 * addToRecentDocuments : String -> String
 *
 * Given a filepath
 * - Add it to native recent documents list (Windows & macOS)
 * - Add it to Home screen documents list
 *
 */

async function addToRecentDocuments (filepath) {
  try {
    await docList.addFileToDocList(filepath);
    await docList.setOpened(filepath);
    app.addRecentDocument(filepath);
  } catch (err) {
    await dialog.showMessageBox(errorAlert("Recent Documents Error", `Couldn't add ${filepath} to recent documents list`, err));
    return;
  }
}


function removeFromRecentDocuments (filepath) {
  try {
    docList.removeDb(filepath);
    app.removeRecentDocument(filepath);
  } catch (err) {
  }
}


function setDocumentChanged(win, changed) {
  let currentTitle = win.getTitle();
  let menuState = docWindowMenuStates[win.id];

  if(changed) {
    if(!currentTitle.startsWith("*")) {
      win.setTitle("*" + currentTitle);
    }

    if (menuState.changed === false) {
      _.set(menuState, "changed", true);
      updateMenu(menuState, false, win);
      win.setDocumentEdited(true);

    }
  } else {
    win.setDocumentEdited(false);
    win.setTitle(currentTitle.replace(/^\*/, ""));

    if (menuState.changed === true) {
      _.set(menuState, "changed", false);
      updateMenu(menuState, false, win);
    }
  }
}



// Quit when all windows are closed.
app.on('window-all-closed', () => {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (_menuQuit || process.platform !== 'darwin') {
    app.quit()
  }
})


app.on('activate', () => {
  if (documentWindows.length == 0) {
    createHomeWindow()
  }
})



ipcMain.on('home:new', async (event) => {
  await newUntitled();
  winHome.close();
})


ipcMain.on('home:import-file', async (event) => {
  let didImport = await importDocument();
  if (didImport) {
    winHome.close();
  }
})


ipcMain.on("home:open", async (event, dbToLoad, docName) => {
  let didOpen = await openDocumentOrFolder(dbToLoad, docName);

  if(didOpen) {
    winHome.close();
  } else {
    winHome.webContents.send("main:doc-list-reload");
  }
});


ipcMain.on("home:open-other", async () => {
  let result = await openWithDialog();

  if(typeof result == "string") {
    winHome.close();
  }
});


ipcMain.on("doc:save-and-exit", async (event) => {
  let docWindow = BrowserWindow.fromWebContents(event.sender);
  let swapFolderPath = _.get(docWindowData, [docWindow.id, "swapFolderPath"]);
  let legacyFormat = _.get(docWindowData, [docWindow.id, "legacyFormat"]);
  let swapStore = new Store({name: "swap", cwd: swapFolderPath})
  let originalPath = swapStore.get("filepath", false);

  try {
    if (originalPath) {
        await saveDocument(docWindow);
        await fio.deleteSwapFolder(swapFolderPath);
        docWindow.destroy();
    } else if (legacyFormat) {
      // Saved to userData folder, never saved as file
      const legacyOptions =
        { title: "Save To File"
        , type: "info"
        , message:
            [ "A past version of Gingko saved this document to an internal folder."
            , "This led to much confusion, and less control."
            , "Sorry!"
            , ""
            , "Choose a new location for this document."
            ].join("\n")
        , buttons: ["Cancel", "Keep in Legacy Format", "Save As File"]
        , defaultId: 2
        };

      let {response: choice} = await dialog.showMessageBox(docWindow, legacyOptions);

      switch (choice) {
        case 0:
          return;

        case 1:
          docWindow.destroy();
          break;

        case 2:
          let saveLegacyResult = await saveLegacyDocumentAs(docWindow);
          if (saveLegacyResult) {
            if (process.platform === "win32") { docWindow.webContents.send("main:database-close"); }
            await fio.deleteSwapFolder(saveLegacyResult.swapFolderPath);
            docWindow.destroy();
          };
          break;
      }
    } else {
      // Untitled/never-saved document
      const confirmOptions =
        { title: tr.saveChanges[lang]
        , message: tr.saveChangesMsg[lang]
        , type: "warning"
        , buttons: [tr.closeWithoutSaving[lang], tr.cancel[lang], tr.save[lang]]
        , defaultId: 2
        };

      let {response : choice} = await dialog.showMessageBox(docWindow, confirmOptions);

      switch (choice) {
        case 0:
          docWindow.destroy();
          return;
        case 1:
          return;
        case 2: {
          let saveDocAsResult = await saveDocumentAs(docWindow);
          if (saveDocAsResult && saveDocAsResult.swapFolderPath) {
            let newSwapFolderPath = saveDocAsResult.swapFolderPath;
            if (process.platform === "win32") { docWindow.webContents.send("main:database-close"); }
            await fio.deleteSwapFolder(newSwapFolderPath);
            docWindow.destroy();
          }
          break;
        }
      }
    }
  } catch (err) {
    throw err;
  }
})


ipcMain.on('license:serial', (event, msg) => {
  let newEmail = msg[0]
  let newSerial = msg[1]
  if(validSerial(newEmail, newSerial)){
    userStore.set('email', newEmail)
    userStore.set('serial', newSerial)
    winSerial.webContents.send('main:serial-success')
  } else {
    winSerial.webContents.send('main:serial-fail')
  }
})


ipcMain.on('license:open-serial-window', (event, msg) => {
  var parentWindow = BrowserWindow.fromWebContents(event.sender).getParentWindow();
  createSerialWindow(parentWindow, msg)
})



/* ==== Modal Windows ==== */

function createTrialWindow(parentWindow, activations, limit) {
  winTrial = new BrowserWindow(
    { width: 500
    , height: 350
    , backgroundColor: '#fff'
    , modal: true
    , useContentSize: true
    , fullscreenable: false
    , resizable: false
    , parent: parentWindow
    , show: false
    , webPreferences: { nodeIntegration: true }
    })

  var url = `file://${__dirname}/static/trial.html`
  winTrial.removeMenu();
  winTrial.once('ready-to-show', () => {
    winTrial.webContents.send('main:trial-activations', [activations, limit])
    winTrial.show()
  })
  winTrial.on('closed', () => {
    winTrial = null;
  })
  winTrial.loadURL(url)
}


function createSerialWindow(parentWindow, shouldBlock) {
  winSerial = new BrowserWindow(
    { width: 440
    , height: 230
    , resizable: false
    , minimizable: false
    , fullscreenable: false
    , backgroundColor: 'lightgray'
    , modal: true
    , useContentSize: true
    , parent: parentWindow
    , show: false
    , webPreferences: { nodeIntegration: true }
    })

  let email = userStore.get('email', "")
  let storedSerial = userStore.get('serial', "")

  var url = `file://${__dirname}/static/license.html`
  winSerial.removeMenu();

  winSerial.once('ready-to-show', () => {
    if(shouldBlock) { winSerial.webContents.send('prevent-close', true) }
    winSerial.webContents.send('serial-info', [email, storedSerial])
    winSerial.show()
  })

  winSerial.on('closed', () => {
    winSerial = null;
  })

  winSerial.loadURL(url)
}
