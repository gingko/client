const {app, BrowserWindow, dialog, Menu, ipcMain, shell} = require('electron')
import { autoUpdater } from "electron-updater"
const path = require('path')
const sha1 = require('sha1')
const Store = require('electron-store')
const windowStateKeeper = require('electron-window-state')
const dbMapping = require('./shared/db-mapping')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let documentWindows = []
let winTrial, winSerial, winHome
let _isEditMode = false
let _columns = 1
const hiddenStore = new Store({name: "kernel", encryptionKey: "79df64f73eab9bc0d7b448d2008d876e"})
const userStore = new Store({name: "config"})



function createHomeWindow () {
  // Create the browser window.
  winHome = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/static/leaf128.png`
    })

  // and load the html of the home window.
  var url = `file://${__dirname}/static/home.html`

  winHome.loadURL(url)
  winHome.setMenu(null)
}


function createAppWindow (dbname) {
  let mainWindowState = windowStateKeeper(
    { defaultWidth: 1000
    , defaultHeight: 800
    , file: `window-state-${dbname}.json`
    }
  )

  // Create the browser window.
  var win = new BrowserWindow(
    { width: mainWindowState.width
    , height: mainWindowState.height
    , x: mainWindowState.x + (documentWindows.length * 30)
    , y: mainWindowState.y + (documentWindows.length * 30)
    , show: false
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/static/leaf128.png`
    })

  documentWindows.push(win)
  mainWindowState.manage(win);

  // and load the html of the app.
  var url = `file://${__dirname}/static/index.html`

  win.loadURL(url)

  win.on('ready-to-show', () => {
    win.webContents.send('main:start-app', dbname)
    win.show()
  })

  win.on('close', (e) => {
    win.webContents.send('main-exit')
    //e.preventDefault()
  })

  // Emitted when the window is closed.
  win.on('closed', () => {
    // Dereference the window object
    let index = documentWindows.indexOf(win)
    if (index !== -1) {
      documentWindows.splice(index, 1)
    }
  })


  // menu is defined outside this function, far below for now.
  Menu.setApplicationMenu(menu)
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
      createTrialWindow(activations, limit)
    }
  }
})


// Quit when all windows are closed.
app.on('window-all-closed', () => {
  // On macOS it is common for applications and their menu bar
  // to stay active until the user quits explicitly with Cmd + Q
  if (process.platform !== 'darwin') {
    app.quit()
  }
})


app.on('activate', () => {
  if (documentWindows.length == 0) {
    createHomeWindow()
  }
})



ipcMain.on('home:new', (event) => {
  let dbname = dbMapping.newDb()
  createAppWindow(dbname)
  winHome.close()
})


ipcMain.on('home:load', (event, dbToLoad) => {
  createAppWindow(dbToLoad)
  winHome.close()
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
  createSerialWindow(msg)
})



/* ==== Modal Windows ==== */

function createTrialWindow(activations, limit) {
  winTrial = new BrowserWindow(
    { width: 500
    , height: 350
    , backgroundColor: '#fff'
    , modal: true
    , useContentSize: true
    , fullscreenable: false
    , resizable: false
    , parent: win
    , show: false
    })

  var url = `file://${__dirname}/static/trial.html`
  winTrial.setMenu(null)
  winTrial.once('ready-to-show', () => {
    winTrial.webContents.send('trial-activations', [activations, limit])
    winTrial.show()
  })
  winTrial.loadURL(url)

  winTrial.on
}


function createSerialWindow(shouldBlock) {
  winSerial = new BrowserWindow(
    { width: 440
    , height: 230
    , resizable: false
    , minimizable: false
    , fullscreenable: false
    , backgroundColor: 'lightgray'
    , modal: true
    , useContentSize: true
    , parent: win
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
  winSerial.loadURL(url)
}




/* ==== Menu ==== */

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


  return [ { label: 'File'
    , submenu:
        [ { label: 'New'
          , accelerator: 'CmdOrCtrl+N'
          , click (item, focusedWindow) {
              let dbname = dbMapping.newDb()
              createAppWindow(dbname)
            }
          }
        , { label: 'Open File...'
          , accelerator: 'CmdOrCtrl+O'
          , click (item, focusedWindow) {
              createHomeWindow()
            }
          }
        , { type: 'separator' }
        , { label: 'Save'
          , accelerator: 'CmdOrCtrl+S'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-save')
            }
          }
        , { label: 'Save As...'
          , accelerator: 'CmdOrCtrl+Shift+S'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-save-as')
            }
          }
        , { type: 'separator' }
        , { label: 'Import JSON File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-import-json')
            }
          }
        , { type: 'separator' }
        , { label: 'Export to MS Word'
          , submenu : exportMenu('docx', cols)
          }
        , { label: 'Export to Text'
          , submenu : exportMenu('txt', cols)
          }
        , { label: 'Export to JSON...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-json')
            }
          }
        , { type: 'separator' }
        , { label: 'Exit...'
          , role: 'quit'
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
              createSerialWindow(false)
            }
          }
          , { type: 'separator' }
          , { label: 'Show Dev Tools'
            , accelerator: process.platform === 'darwin' ? 'Alt+Command+I' : 'Ctrl+Shift+I'
            , click (item, focusedWindow) {
                if (focusedWindow) focusedWindow.webContents.toggleDevTools()
              }
            }
        ]
    }
  ]
}

var menuTemplate = menuFunction(_isEditMode, _columns)

var menu = Menu.buildFromTemplate(menuTemplate)
