const {app, BrowserWindow, dialog, Menu, ipcMain, shell, Notification} = require('electron')
import { autoUpdater } from "electron-updater"
const fs = require('fs-extra')
const path = require('path')
const child_process = require('child_process')
const sha1 = require('sha1')
const machineIdSync = require('node-machine-id').machineIdSync
const moment = require('moment')
const Store = require('electron-store')
import { path7za } from '7zip-bin'
import TurndownService from 'turndown'
const windowStateKeeper = require('electron-window-state')
const dbMapping = require('./shared/db-mapping')
const fio = require('./electron/file-io')
const GingkoError  = require("./shared/errors");
const errorAlert = require('./shared/shared').errorAlert

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let documentWindows = []
let winTrial, winSerial, winHome, winRename
let _untitledDocs = 0
let _documentFocused = false
let _isEditMode = false
let _columns = 1
let _hasLastExport = false
let _menuQuit = false
const hiddenStore = new Store({name: "kernel", encryptionKey: "79df64f73eab9bc0d7b448d2008d876e"})
const userStore = new Store({name: "config"})


// Make Gingko single instance
const isSecondInstance = app.makeSingleInstance((commandLine, workingDirectory) => {
  if(commandLine[0].endsWith('electron') && typeof commandLine[2] == 'string') {
    openDocument(commandLine[2])
  } else if (!!winHome) {
    winHome.show()
  } else {
    createHomeWindow()
  }
})
if (isSecondInstance) { app.exit() }



function createHomeWindow () {
  // Create the browser window.
  winHome = new BrowserWindow(
    { width: 640
    , height: 600
    , backgroundColor: '#477085'
    , icon: `${__dirname}/static/leaf128.png`
    })

  // and load the html of the home window.
  var url = `file://${__dirname}/static/home.html`

  winHome.loadURL(url)

  winHome.on('focus', () => {
    _documentFocused = false;
    buildMenu();
    winHome.setMenu(null)
  })

  winHome.on('closed', () => {
    winHome = null;
  })

  buildMenu();
  winHome.setMenu(null)
}


function createDocumentWindow (swapFolderPath, originalPath) {
  let metaStore = new Store({name: 'meta', cwd: swapFolderPath})

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
    })

  documentWindows.push(win);

  mainWindowState.manage(win);


  // Add swapFolderPath variable to BrowserWindow object,
  // so it can be accessed elsewhere in main.js
  win.swapFolderPath = swapFolderPath;

  // Add dbPath variable to BrowserWindow object,
  // so that it can load from the database as soon as
  // the window is created.
  win.dbPath = path.join(swapFolderPath, 'leveldb');

  if (originalPath) {
    let newTitle = `${path.basename(originalPath)} - Gingko`;
    win.setTitle(newTitle);
  } else {
    let newTitle = "Untitled" + (_untitledDocs !== 0 ? ` (${_untitledDocs + 1})` : "");
    win.setTitle(newTitle);
    _untitledDocs += 1;
  }
  //win.jsonImportData = jsonImportData;

  var url = `file://${__dirname}/static/index.html`
  win.loadURL(url)

  win.on('ready-to-show', () => {
    win.show()
  })

  win.on('focus', () => {
    _documentFocused = true;
    buildMenu();
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


function createRenameWindow(parentWindow, dbName, currentName, closeDocument) {
  winRename = new BrowserWindow(
  { width: 440
  , height: 140
  , resizable: false
  , minimizable: false
  , fullscreenable: false
  , backgroundColor: 'lightgray'
  , modal: true
  , parent: parentWindow
  , useContentSize: true
  , show: false
  })

  winRename.setMenu(null)

  winRename.once('ready-to-show', () => {
    winRename.show()
  })

  winRename.on('closed', () => {
    winRename = null;
  })

  var url = `file://${__dirname}/static/rename.html`
  winRename.loadURL(url)

  if (!!currentName) {
    winRename.currentName = currentName;
  } else {
    winRename.currentName = "Untitled";
  }
  winRename.dbName = dbName;
  winRename.closeDocument = closeDocument;
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




function buildMenu() {
  let editMenu

  if (_isEditMode || !_documentFocused) {
    editMenu =
      { label: 'Edit'
      , submenu:
          [ { role: 'undo' }
          , { role: 'redo' }
          , { type: 'separator' }
          , { role: 'cut' }
          , { role: 'copy' }
          , { role: 'paste' }
          , { role: 'selectAll'}
          ]
      }
  } else {
    editMenu =
      { label: 'Edit'
      , submenu:
          [ { label: 'Undo'
            , enabled : false
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('undo')
                }
              }
            , accelerator : 'CommandOrControl+Z'
            }
          , { label: 'Redo'
            , enabled : false
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('redo')
                }
              }
            , accelerator : 'CommandOrControl+Shift+Z'
            }
          , { type: 'separator' }
          , { label: 'Cut Cards'
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('menu-cut')
                }
              }
            , accelerator : 'CommandOrControl+X'
            }
          , { label: 'Copy Cards'
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('menu-copy')
                }
              }
            , accelerator : 'CommandOrControl+C'
            }
          , { label: 'Paste Cards'
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('menu-paste')
                }
              }
            , accelerator : 'CommandOrControl+V'
            }
          , { label: 'Paste Cards as Children'
            , click (item, focusedWindow) {
                if (process.platform !== 'darwin') {
                  focusedWindow.webContents.send('menu-paste-into')
                }
              }
            , accelerator : 'CommandOrControl+Shift+V'
            }
          ]
      }
  }

  var exportMenu = function(format, cols) {
    var expMenu =
      [ { label : 'Entire Document...'
        , click (item, focusedWindow) {
            focusedWindow.webContents.send(`menu-export-${format}`)
          }
        }
      , { label : 'Current Card and Children...'
        , click (item, focusedWindow) {
            focusedWindow.webContents.send(`menu-export-${format}-current`)
          }
        }
      , { type: 'separator' }
      ]

    var expMenuItem = function (num) {
      return  { label : `Column ${num}...`
              , click (item, focusedWindow) {
                  focusedWindow.webContents.send(`menu-export-${format}-column`, num)
                }
              }
    }

    for (var i = 1; i <= cols;i++) {
      expMenu.push(expMenuItem(i))
    }

    return expMenu
  }

  var menuTemplate =
    [ { label: 'File'
    , submenu:
        [ { label: 'New'
          , accelerator: 'CmdOrCtrl+N'
          , click (item, focusedWindow) {
              newUntitled();
            }
          }
        , { label: 'Open File...'
          , accelerator: 'CmdOrCtrl+O'
          , click (item, focusedWindow) {
              let options = {title: 'Open File...', defaultPath : app.getPath('documents') , properties: ['openFile'], filters: [ {name: 'Gingko Files (*.gko)', extensions: ['gko']} ]};
              dialog.showOpenDialog(options, (filepaths) => {
                if(Array.isArray(filepaths) && !!filepaths[0]) {
                  openDocument(filepaths[0])
                }
              })
            }
          }
        , { label: 'Save'
          , enabled: _documentFocused // TODO: Disable this for Untitled documents
          , accelerator: 'CmdOrCtrl+S'
          , click (item, focusedWindow) {
              saveDocument(focusedWindow);
            }
          }
        , { label: 'Save As'
          , enabled: _documentFocused
          , accelerator: 'CmdOrCtrl+Shift+S'
          , click (item, focusedWindow) {
              saveDocumentAs(focusedWindow);
            }
          }
        , { type: 'separator' }
        , { label: 'Import JSON File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-import-json')
            }
          }
        , { type: 'separator' }
        , { label: 'Export as MS Word'
          , enabled: _documentFocused
          , submenu : exportMenu('docx', _columns)
          }
        , { label: 'Export as Text'
          , enabled: _documentFocused
          , submenu : exportMenu('txt', _columns)
          }
        , { label: 'Export as JSON...'
          , enabled: _documentFocused
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-json')
            }
          }
        , { label: 'Repeat Last Export'
          , enabled: _hasLastExport && _documentFocused
          , accelerator: 'CommandOrControl+r'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-repeat')
            }
          }
        ]
    }
  , editMenu
  , { label: 'View'
    , submenu:
        [ { label: 'Zoom In'
          , enabled: _documentFocused
          , accelerator: 'CommandOrControl+='
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('zoomin')
            }
          }
        , { label: 'Zoom Out'
          , enabled: _documentFocused
          , accelerator: 'CommandOrControl+-'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('zoomout')
            }
          }
        , { label: 'Reset Zoom'
          , enabled: _documentFocused
          , accelerator: 'CommandOrControl+0'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('resetzoom')
            }
          }
        , { type: 'separator' }
        , { role: 'togglefullscreen' }
        ]
    }
  , { label: 'Help'
    , submenu:
        [ { label: 'FAQ...'
          , click (item, focusedWindow) {
              shell.openExternal('https://gingko.io/faq.html')
            }
          }
        , { label: 'Features List && Known Bugs...'
          , click (item, focusedWindow) {
              shell.openExternal('https://github.com/gingko/client/issues')
            }
          }
        , { label: 'Contact Adriano...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-contact-support')
            }
          }
        , { type: 'separator' }
        , { label: 'Buy a License...'
          , click (item, focusedWindow) {
              shell.openExternal('https://gingkoapp.com/desktop-upgrade')
            }
          }
        , { label: 'Enter License...'
          , click (item, focusedWindow) {
              createSerialWindow(focusedWindow, false)
            }
          }
          , { type: 'separator' }
          , { label: 'Open Debugging Tools'
            , accelerator: process.platform === 'darwin' ? 'Alt+Command+I' : 'Ctrl+Shift+I'
            , click (item, focusedWindow) {
                if (focusedWindow) focusedWindow.webContents.toggleDevTools()
              }
            }
        ]
    }
  ]


  if (process.platform == 'darwin') {
    menuTemplate.unshift(
      { label : app.getName()
      , submenu :
          [ {role: 'about'}
          , {type: 'separator'}
          , {role: 'services', submenu: []}
          , {type: 'separator'}
          , {role: 'hide'}
          , {role: 'hideothers'}
          , {role: 'unhide'}
          , {type: 'separator'}
          , { label: 'Quit Gingko...'
            , accelerator: 'Command+Q'
            , click (item, focusedWindow, event) {
                _menuQuit = true;
                app.quit();
              }
            }
          ]
      })

    menuTemplate.splice(4, 0, { role: 'windowMenu'})
  } else {
    let closeMenuItem = { label : 'Close', accelerator: 'Ctrl+W', click (item, focusedWindow) { focusedWindow.webContents.send('menu-close-document'); }};
    menuTemplate[0].submenu.splice(3, 0, closeMenuItem);
    menuTemplate[0].submenu.push({type: 'separator'}, {role: 'quit'} );
  }


  let menu = Menu.buildFromTemplate(menuTemplate)
  Menu.setApplicationMenu(menu)
}

buildMenu();




/* ==== App Events ==== */

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', () => {

  // Auto Updater code
  autoUpdater.fullChangelog = true;

  autoUpdater.on('update-downloaded', info => {
    let turndownService = new TurndownService();
    if (Array.isArray(info.releaseNotes)){
      var releaseNotesText = info.releaseNotes.map(rn => {
        return turndownService.turndown(rn.note);
      }).join('\n').replace(/\*/g, '·');
    } else {
      var releaseNotesText = turndownService.turndown(info.releaseNotes).replace(/\*/g, '·');
    }

    let updateNotification = new Notification(
      { title: `${app.getName()} will be updated to v${info.version} on exit`
        , body: `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">Change list</a>:\n${releaseNotesText}`
      });

    updateNotification.show();
  });

  autoUpdater.checkForUpdates()

  let email = userStore.get('email', "")
  let storedSerial = userStore.get('serial', "")

  if(process.argv[0].endsWith('electron') && typeof process.argv[2] == 'string') {
    openDocument(process.argv[2]);
  } else {
    createHomeWindow()
    if(!validSerial(email, storedSerial)) {
      let activations = getTrialActivations()
      let limit = 30
      let daysLeft = Math.max(limit - activations.length, 0)
      let trialDisplayDays = [25, 20, 15, 10, 8, 6, 5, 4, 3, 2, 1, 0]

      if(trialDisplayDays.includes(daysLeft)) {
        createTrialWindow(winHome, activations, limit)
      }
    }
  }
})


function newUntitled() {
  let swapRandomName = sha1(Date.now()+machineIdSync()).slice(20)
  createDocumentWindow(path.join(app.getPath('userData'), swapRandomName), null)
}


async function openDocument(filepath) {
  try {
    let swapFolderPath = await fio.openFile(filepath);
    createDocumentWindow(swapFolderPath, filepath);
    app.addRecentDocument(filepath);
  } catch (err) {
    console.log(err);
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
  try {
    const filepath = await fio.saveSwapFolder(docWindow.swapFolderPath);
    app.addRecentDocument(filepath);
    return filepath;
  } catch (err) {
    throw err;
  }
}




/*
 * saveDocumentAs : BrowserWindow -> Promise String Error
 *
 * Given a docWindow
 * - Get a newFilepath with save dialog
 * - Call saveSwapFolderAs to copy swap folder and save it
 * - Set docWindow.swapFolderPath and docWindow's title
 * - Send "set-swap-folder" message to doc.js
 * Return new filepath if successful.
 *
 */

async function saveDocumentAs (docWindow) {
  let saveOptions =
    { title: "Save As"
    , defaultPath: path.join(app.getPath("documents"), "Untitled.gko")
    , filters: [{ name: "Gingko Files (*.gko)", extensions: ["gko"] }]
    };

  const newFilepath = dialog.showSaveDialog(docWindow, saveOptions);

  if (newFilepath) {
    try {
      const newSwapFolderPath = await fio.saveSwapFolderAs(docWindow.swapFolderPath, newFilepath);
      docWindow.swapFolderPath = newSwapFolderPath;
      app.addRecentDocument(newFilepath);
      docWindow.setTitle(`${path.basename(newFilepath)} - Gingko`);
      docWindow.webContents.send("main:set-swap-folder", newSwapFolderPath);
      return newFilepath;
    } catch (err) {
      throw err;
    }
  }
}




function saveFileOld(swapFolderPath, targetPath) {
  return new Promise(
    (resolve, reject) => {
      // Remove swap-only data
      fs.unlinkSync(path.join(swapFolderPath, 'swap.json'))

      // Convert swap folder to new .gko backup file by
      // zip archiving it...
      let backupPath = swapFolderPath + moment().format('_YYYY-MM-DD_HH-MM-SS') + '.gko';
      let args =
          ['a'
          , backupPath
          , swapFolderPath + path.sep + '*'
          , '-r'
          ]

      child_process.execFile(path7za, args, (err) => {
        if (err) { reject(err) }

        // ... and when done, copy to target path,
        // overwriting original .gko file with new one...
        fs.copyFile(backupPath, targetPath, (err) => {
          if(err) { reject(err)}

          // ... and finally, delete the swap folder.
          fs.remove(swapFolderPath, err => {
            console.log('clean close', targetPath)
            app.addRecentDocument(targetPath)
            resolve(targetPath)
          });
        })
      })
    })
}



// Quit when all windows are closed.
app.on('window-all-closed', () => {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (_menuQuit || process.platform !== 'darwin') {
    app.quit()
  } else {
    _documentFocused = false;
  }
})


app.on('activate', () => {
  if (documentWindows.length == 0) {
    createHomeWindow()
  }
})



ipcMain.on('home:new', (event) => {
  newUntitled();
  winHome.close();
})


ipcMain.on('home:import-file', async (event) => {
  var options =
      { title: 'Open File...'
      , defaultPath: app.getPath('documents')
      , properties: ['openFile']
      , filters:  [ {name: 'Gingko Desktop Files (*.gko)', extensions: ['gko']}
                  , {name: 'Gingko JSON Files (*.json)', extensions: ['json']}
                  , {name: 'All Files', extensions: ['*']}
                  ]
      }

  var filepathArray = dialog.showOpenDialog(winHome, options)
  if (!!filepathArray) {
    try {
      var { dbName, docName, jsonImportData } = await fio.dbFromFile(filepathArray[0])
    } catch (err) {
      dialog.showMessageBox(errorAlert("Import Error", "Couldn't load .gko data", err))
      return;
    }
    dbMapping.newDb(dbName, docName)
    createDocumentWindow(dbName, docName, jsonImportData)
    winHome.close()
  }
})


ipcMain.on('home:load', (event, dbToLoad, docName) => {
  let swapPath = path.join(app.getPath('userData'), dbToLoad);
  createDocumentWindow(swapPath, null)
  winHome.close()
})


ipcMain.on('home:delete', async (event, dbToDelete) => {
  await fio.destroyDb(dbToDelete)
  await dbMapping.removeDb(dbToDelete)
})


ipcMain.on("app:close", async (event) => {
  let docWindow = BrowserWindow.fromWebContents(event.sender);
  let swapFolderPath = docWindow.swapFolderPath;
  let swapStore = new Store({name: "swap", cwd: swapFolderPath})
  let originalPath = swapStore.get("filepath", false);

  try {
    if (originalPath) {
        await saveDocument(docWindow);
        await fio.deleteSwapFolder(swapFolderPath);
        docWindow.destroy();
    } else {
      // Untitled/never-saved document
      const confirmOptions =
        { title: "Save changes"
        , message: "Save changes before closing?"
        , buttons: ["Close Without Saving", "Cancel", "Save"]
        , defaultId: 2
        }
      let choice = dialog.showMessageBox(confirmOptions)

      switch (choice) {
        case 0:
          docWindow.destroy();
          return;
        case 1:
          return;
        case 2:
          let newSwapFolderPath = await saveDocumentAs(docWindow);
          await fio.deleteSwapFolder(newSwapFolderPath);
          docWindow.destroy();
          break;
      }
    }
  } catch (err) {
    throw err;
  }
})


ipcMain.on("doc:set-changed", (event, changed) => {
  let docWindow = BrowserWindow.fromWebContents(event.sender);
  let currentTitle = docWindow.getTitle();

  docWindow.setDocumentEdited(changed);

  if(changed && !currentTitle.startsWith("*")) {
    docWindow.setTitle("*" + currentTitle);
  } else if (!changed) {
    docWindow.setTitle(currentTitle.replace(/^\*/, ""));
  }
})


ipcMain.on('app:rename-untitled', (event, dbName, currName, closeDocument) => {
  createRenameWindow(BrowserWindow.fromWebContents(event.sender), dbName, currName, closeDocument)
})


ipcMain.on('app:last-export-set', (event, lastPath) => {
  _hasLastExport = !!lastPath
  buildMenu();
})


ipcMain.on('rename:renamed', (event, dbName, newName, closeDocument) => {
  let renameWindow = BrowserWindow.fromWebContents(event.sender);
  let appWindow = renameWindow.getParentWindow();

  dbMapping.renameDoc(dbName, newName)
  appWindow.renameDoc(newName)

  if (closeDocument) {
    appWindow.destroy();
  }
})

ipcMain.on('rename:delete-and-close', (event, dbToDelete) => {
  let renameWindow = BrowserWindow.fromWebContents(event.sender);
  let appWindow = renameWindow.getParentWindow();

  appWindow.webContents.send('main:delete-and-close')
})


ipcMain.on('column-number-change', (event, cols) => {
  if (_columns != cols) {
    _columns = cols
    buildMenu();
  }
})

ipcMain.on('edit-mode-toggle', (event, isEditing) => {
  if (_isEditMode != isEditing) {
    _isEditMode = isEditing
    buildMenu();
  }
})


ipcMain.on('serial', (event, msg) => {
  let newEmail = msg[0]
  let newSerial = msg[1]
  if(validSerial(newEmail, newSerial)){
    userStore.set('email', newEmail)
    userStore.set('serial', newSerial)
    winSerial.webContents.send('serial-success')
  } else {
    winSerial.webContents.send('serial-fail')
  }
})


ipcMain.on('open-serial-window', (event, msg) => {
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
    })

  var url = `file://${__dirname}/static/trial.html`
  winTrial.setMenu(null)
  winTrial.once('ready-to-show', () => {
    winTrial.webContents.send('trial-activations', [activations, limit])
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
    })

  let email = userStore.get('email', "")
  let storedSerial = userStore.get('serial', "")

  var url = `file://${__dirname}/static/license.html`
  winSerial.setMenu(null)

  winSerial.once('ready-to-show', () => {
    if(shouldBlock) { winSerial.webContents.send('prevent-close', true) }
    winSerial.webContents.send('serial-info', [email, storedSerial])
    winSerial.show()
  })

  winSerial.on('focus', () => {
    _documentFocused = false;
    buildMenu();
  })

  winSerial.on('closed', () => {
    winSerial = null;
  })

  winSerial.loadURL(url)
}
