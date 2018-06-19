const {app, BrowserWindow, dialog, Menu, ipcMain, shell} = require('electron')
import { autoUpdater } from "electron-updater"
const path = require('path')
const sha1 = require('sha1')
const Store = require('electron-store')
const windowStateKeeper = require('electron-window-state')
const dbMapping = require('./shared/db-mapping')
const fio = require('./shared/file-io')
const errorAlert = require('./shared/shared').errorAlert

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let documentWindows = []
let winTrial, winSerial, winHome, winRename
let _isEditMode = false
let _columns = 1
let _menuQuit = false
const hiddenStore = new Store({name: "kernel", encryptionKey: "79df64f73eab9bc0d7b448d2008d876e"})
const userStore = new Store({name: "config"})



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

  Menu.setApplicationMenu(menu)
  winHome.setMenu(null)

  winHome.on('closed', () => {
    winHome = null;
  })
}


function createAppWindow (dbName, docName, jsonImportData) {
  let mainWindowState = windowStateKeeper(
    { defaultWidth: 1000
    , defaultHeight: 800
    , file: `window-state-${dbName}.json`
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


  // Add variables to BrowserWindow object, so they can be
  // accessed from the app window
  win.dbName = dbName;
  win.docName = docName;
  win.jsonImportData = jsonImportData;

  win.renameDoc = function(newName) {
    win.setTitle(`${newName} - Gingko`)
    win.docName = newName
  }

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


function exportMenu(format, cols) {
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


function menuFunction(isEditing, cols) {
  let editMenu

  if (isEditing) {
    editMenu =
      { label: 'Edit'
      , submenu:
          [ { role: 'undo' }
          , { role: 'redo' }
          , { type: 'separator' }
          , { role: 'cut' }
          , { role: 'copy' }
          , { role: 'paste' }
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


  var defaultMenu =
    [ { label: 'File'
    , submenu:
        [ { label: 'New'
          , accelerator: 'CmdOrCtrl+N'
          , click (item, focusedWindow) {
              let dbName = dbMapping.newDb()
              createAppWindow(dbName)
            }
          }
        , { label: 'Open File...'
          , accelerator: 'CmdOrCtrl+O'
          , click (item, focusedWindow) {
              createHomeWindow()
            }
          }
        , { type: 'separator' }
        , { label: 'Rename...'
          , click (item, focusedWindow) {
              createRenameWindow(focusedWindow, focusedWindow.dbName, focusedWindow.docName, false)
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
          , submenu : exportMenu('docx', cols)
          }
        , { label: 'Export as Text'
          , submenu : exportMenu('txt', cols)
          }
        , { label: 'Export as JSON...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-json')
            }
          }
        ]
    }
  , editMenu
  , { label: 'View'
    , submenu:
        [ { label: 'Zoom In'
          , accelerator: 'CommandOrControl+='
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('zoomin')
            }
          }
        , { label: 'Zoom Out'
          , accelerator: 'CommandOrControl+-'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('zoomout')
            }
          }
        , { label: 'Reset Zoom'
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
        [ { label: 'FAQ'
          , click (item, focusedWindow) {
              shell.openExternal('https://gingko.io/faq.html')
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
    defaultMenu.unshift(
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

    defaultMenu.splice(4, 0, { role: 'window'})
  } else {
    defaultMenu[0].submenu.push({type: 'separator'} , {role: 'quit'} )
  }


  return defaultMenu;
}


/* ==== App Events ==== */

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', () => {
  autoUpdater.checkForUpdatesAndNotify();

  let email = userStore.get('email', "")
  let storedSerial = userStore.get('serial', "")

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
})


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



ipcMain.on('home:new', (event) => {
  let dbName = dbMapping.newDb()
  createAppWindow(dbName, null)
  winHome.close()
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
    createAppWindow(dbName, docName, jsonImportData)
    winHome.close()
  }
})


ipcMain.on('home:load', (event, dbToLoad, docName) => {
  createAppWindow(dbToLoad, docName)
  winHome.close()
})


ipcMain.on('home:delete', async (event, dbToDelete) => {
  await fio.destroyDb(dbToDelete)
  await dbMapping.removeDb(dbToDelete)
})


ipcMain.on('app:rename-untitled', (event, dbName, currName, closeDocument) => {
  createRenameWindow(BrowserWindow.fromWebContents(event.sender), dbName, currName, closeDocument)
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
    menuTemplate = menuFunction(_isEditMode, _columns)
    menu = Menu.buildFromTemplate(menuTemplate)
    Menu.setApplicationMenu(menu)
  }
})

ipcMain.on('edit-mode-toggle', (event, isEditing) => {
  if (_isEditMode != isEditing) {
    _isEditMode = isEditing
    menuTemplate = menuFunction(_isEditMode, _columns)
    menu = Menu.buildFromTemplate(menuTemplate)
    Menu.setApplicationMenu(menu)
  }
})


ipcMain.on('elm-ready', () => {
  if(process.argv[0].endsWith('electron')) {
    if(typeof process.argv[2] == 'string') {
      win.webContents.send('open-file', process.argv[2])
    }
  } else {
    if(typeof process.argv[1] == 'string') {
      win.webContents.send('open-file', process.argv[1])
    }
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
  winSerial.on('closed', () => {
    winSerial = null;
  })
  winSerial.loadURL(url)
}




/* ==== Menu ==== */

var menuTemplate = menuFunction(_isEditMode, _columns)

var menu = Menu.buildFromTemplate(menuTemplate)
