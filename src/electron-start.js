const {app, BrowserWindow, dialog, Menu, ipcMain, shell, autoUpdater} = require('electron')
const {Feed} = require('dblsqd-sdk')
const {UpdateWindow} = require('dblsqd-electron')
const fs = require('mz/fs')
const path = require('path')
const sha1 = require('sha1')
const Store = require('electron-store')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let win, updateWindow, winTrial, winSerial
let changed = false
const hiddenStore = new Store({name: "kernel", encryptionKey: "79df64f73eab9bc0d7b448d2008d876e"})
const userStore = new Store({name: "config"})

if(require('electron-squirrel-startup')) app.quit()




function createAppWindow () {

  // Create the browser window.
  win = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/static/leaf128.png`
    })


  // and load the html of the app.
  var url = `file://${__dirname}/static/index.html`

  if (process.platform !== 'darwin') {
    if(!!process.argv[1] && !process.argv[0].endsWith('electron')) {
      url += `#${encodeURIComponent(process.argv[1])}`
    }
  } else {
    if(!!process.argv[2] && !process.argv[0].endsWith('electron')) {
      url += `#${encodeURIComponent(process.argv[2])}`
    }
  }
  win.loadURL(url)

  win.on('close', (e) => {
    if (changed) {
      win.webContents.send('main-save-and-close')
      e.preventDefault()
    }
  })

  // Emitted when the window is closed.
  win.on('closed', () => {

    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    win = null
  })



  // menu is defined outside this function, far below for now.
  Menu.setApplicationMenu(menu)
}

function getTrialActivations() {
  let activations = hiddenStore.get('activations', []).concat((new Date).toISOString()).slice(0,10)
  let uniqueActivations = Array.from(new Set(activations.map(d => d.substring(0,10))))
  if(activations !== uniqueActivations) {
    hiddenStore.set('activations', uniqueActivations)
  }
  return uniqueActivations
}


function validSerial(email, storedSerial) {
  let hash = sha1(email + "I know you can hack this, but please don't! If you really can't afford Gingko, just get in touch with me.")
  let serial = [hash.substr(4,4), hash.substr(12,4), hash.substr(20,4), hash.substr(28,4)].join("-").toUpperCase()
  return storedSerial == serial
}



/* ==== App Events ==== */

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', () => {
  let email = userStore.get('email', "")
  let storedSerial = userStore.get('serial', "")

  createAppWindow()
  if(!validSerial(email, storedSerial)) {
    let activations = getTrialActivations()
    let limit = 30
    let trialDisplayDays = [29, 25, 20, 15, 10, 8, 6, 5, 4, 3, 2, 1, 0]

    if(trialDisplayDays.includes(Math.max(limit - activations.length, 0))) {
      createTrialWindow(activations, limit)
    }
  }
  createUpdateWindow()
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
  // On macOS it's common to re-create a window in the app when the
  // dock icon is clicked and there are no other windows open.
  if (win === null) {
    createAppWindow()
  }
})


ipcMain.on('changed', (event, msg) => {
  changed = msg;
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

function createUpdateWindow() {
  const feed = new Feed('https://feeds.dblsqd.com/IEsVYK1_Te2BZIyJWhcelw', 'release')
  const usingAppImage = process.platform == 'linux'

  updateWindow = new UpdateWindow(feed, {
    icon: `${__dirname}/static/leaf128.png`,
    showOn: 'update',
    saveUpdateFile: usingAppImage,
    startUpdateAsProcess: usingAppImage
  })
}


function createTrialWindow(activations, limit) {
  winTrial = new BrowserWindow(
    { width: 500
    , height: 350
    , backgroundColor: '#fff'
    , modal: true
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
    { width: 450
    , height: 262
    , resizable: false
    , minimizable: false
    , fullscreenable: false
    , backgroundColor: 'lightgray'
    , modal: true
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

const menuTemplate =
  [ { label: 'File'
    , submenu:
        [ { label: 'New'
          , accelerator: 'CmdOrCtrl+N'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-new')
            }
          }
        , { label: 'Open File...'
          , accelerator: 'CmdOrCtrl+O'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-open')
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
        , { label: 'Export JSON File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-json')
            }
          }
        , { label: 'Export Text File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-txt')
            }
          }
        , { type: 'separator' }
        , { label: 'Exit...'
          , role: 'quit'
          }
        ]
    }
  , { label: 'Edit'
    , submenu:
        [ { label: 'Undo'
          , enabled : false
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('undo')
            }
          }
        , { label: 'Redo'
          , enabled : false
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('redo')
            }
          }
        , { type: 'separator' }
        , { label: "Cut", accelerator: "CmdOrCtrl+X", selector: "cut:" }
        , { label: "Copy", accelerator: "CmdOrCtrl+C", selector: "copy:" }
        , { label: "Paste", accelerator: "CmdOrCtrl+V", selector: "paste:" }
        , { label: "Select All", accelerator: "CmdOrCtrl+A", selector: "selectAll:" }
        ]
    }
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
        [ { label: 'Contact Adriano'
          , click (item, focusedWindow) {
              shell.openExternal('mailto:adriano@gingkoapp.com')
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
        ]
    }
  ]

if(true) {//process.defaultApp || /[\\/]electron-prebuilt[\\/]/.test(process.execPath) || /[\\/]electron[\\/]/.test(process.execPath)) {
  menuTemplate.push(
  { label: 'Debug'
    , submenu:
        [ { label: 'Show Dev Tools'
          , accelerator: process.platform === 'darwin' ? 'Alt+Command+I' : 'Ctrl+Shift+I'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.toggleDevTools()
            }
          }
        ]
    }
 )
}

const menu = Menu.buildFromTemplate(menuTemplate)
