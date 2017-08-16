const {app, BrowserWindow, dialog, Menu, ipcMain, shell} = require('electron')
const fs = require('mz/fs')
const path = require('path')
const sha1 = require('sha1')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let win
let listWindow
let serialWindow
let changed = false


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

// This method will be called when Electron has finished
// initialization and is ready to create browser windows.
// Some APIs can only be used after this event occurs.
app.on('ready', createAppWindow)

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

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

ipcMain.on('changed', (event, msg) => {
  changed = msg;
})


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
        , { label: 'Export JSON File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-export-json')
            }
          }
        , { label: 'Import JSON File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('menu-import-json')
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
              focusedWindow.webContents.send('contact-support')
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
