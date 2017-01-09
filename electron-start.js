const {app, BrowserWindow, dialog, Menu, ipcMain, shell} = require('electron')
const fs = require('fs')
const sha1 = require('sha1')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let win
let serialWindow
let saved = true

function createWindow () {

  // Create the browser window.
  win = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/dist/leaf128.png` 
    })


  // and load the index.html of the app.
  if (process.platform !== 'darwin') {
    if(!!process.argv[1] && !process.argv[0].endsWith('electron')) {
      win.loadURL(`file://${__dirname}/dist/index.html#${encodeURIComponent(process.argv[1])}`)
    } else {
      win.loadURL(`file://${__dirname}/dist/index.html`)
    }
  } else {
    if(!!process.argv[2] && !process.argv[0].endsWith('electron')) {
      win.loadURL(`file://${__dirname}/dist/index.html#${encodeURIComponent(process.argv[2])}`)
    } else {
      win.loadURL(`file://${__dirname}/dist/index.html`)
    }
  }

  win.on('close', (e) => {
    if(!saved) {
      var options = 
        { title: "Save changes?"
        , message: "Save changes before closing?"
        , buttons: ["Close Without Saving", "Cancel", "Save"]
        , defaultId: 2
        }
      var choice = dialog.showMessageBox(options)

      switch (choice) {
        case 0:
          win.webContents.send('clear-swap')
          break
        case 1:
          e.preventDefault()
          break
        case 2:
          win.webContents.send('clear-swap')
          win.webContents.send('save-and-close')
          e.preventDefault()
          break
      }
    } else {
      win.webContents.send('clear-swap')
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
app.on('ready', createWindow)

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
    createWindow()
  }
})

// In this file you can include the rest of your app's specific main process
// code. You can also put them in separate files and require them here.

ipcMain.on('saved', (event, msg) => {
  saved = msg;
})

ipcMain.on('ask-for-email', (event, msg) => {
  showEmailWindow()
})

ipcMain.on('request-message', (event, msg) => {
  var options =
        { title: "Support Gingko's Developer"
        , message: 
"Thank you so much for trying Gingko!\n\n\
\
Gingko is the work of one person,\n\
and it's how I support myself and my family.\n\n\
If you've found it useful, please consider contributing.\
"
        , icon: `${__dirname}/dist/leaf128.png` 
        , buttons: ['Support Gingko', 'Maybe Later']
        , defaultId: 0
        , cancelId: 1
        }

      dialog.showMessageBox(win
                           , options
                           , res => {
                               if(res == 0) openPaymentPage()
                           })
})

ipcMain.on('id-info', (event, msg) => {
  win.webContents.send('id-info', msg)
})

ipcMain.on('serial', (event, msg) => {
  var hash = sha1(msg+"Please don't steal. Just contact me if you need this for free.")
  if(hash == '3ac67309d2ff5dd533644c9d82d7359f5f729930') {
    win.send('serial-success')
    serialWindow.send('serial-success')
  } else {
    serialWindow.send('serial-fail')
  }
})

openPaymentPage = () => {
  shell.openExternal('https://gingkoapp.com/2a0215') 
}

showSerialWindow = () => {
  serialWindow = new BrowserWindow(
    { parent: win
    , modal: true
    , show: false
    , width: 400
    , height: 80
    , backgroundColor: '#ccc'
    , resizable: false
    , minimizable: false
    , maximizable: false
    , fullscreenable: false
    }
  )
  serialWindow.setMenu(null)
  serialWindow.loadURL(`file://${__dirname}/dist/request.html`)
  serialWindow.once('ready-to-show', () => {
    serialWindow.show()
  })
}

showEmailWindow = () => {
  emailWindow = new BrowserWindow(
    { parent: win
    , modal: true
    , show: false
    , width: 400
    , height: 80
    , backgroundColor: '#ccc'
    }
  )
  emailWindow.setMenu(null)
  emailWindow.loadURL(`file://${__dirname}/dist/email.html`)
  emailWindow.once('ready-to-show', () => {
    emailWindow.show()
  })
}

const menuTemplate = 
  [ { label: 'File'
    , submenu:
        [ { label: 'New'
          , accelerator: 'CmdOrCtrl+N'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('new')
            }
          }
        , { label: 'Open File...'
          , accelerator: 'CmdOrCtrl+O'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('open')
            }
          }
        , { label: 'Import File...'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('import')
            }
          }
        , { type: 'separator' }
        , { label: 'Save'
          , accelerator: 'CmdOrCtrl+S'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('save')
            }
          }
        , { label: 'Save As...'
          , accelerator: 'CmdOrCtrl+Shift+S'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('save-as')
            }
          }
        , { label: 'Export as JSON..'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('export-as-json')
            }
          }
        , { label: 'Export as Markdown..'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('export-as-markdown')
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
        [ { label: 'Contact Support'
          , click (item, focusedWindow) {
              focusedWindow.webContents.send('contact-support')
            }
          }
        , { type: 'separator' }
        , { label: 'Purchase License'
          , click (item, focusedWindow) {
              openPaymentPage()
            }
          }
        , { label: 'Enter License Key'
          , click (item, focusedWindow) {
              showSerialWindow()
            }
          }
        ]
    }
  ] 

if(process.defaultApp || /[\\/]electron-prebuilt[\\/]/.test(process.execPath) || /[\\/]electron[\\/]/.test(process.execPath)) {
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
