const {app, BrowserWindow, dialog, Menu, ipcMain} = require('electron')
const fs = require('fs')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let win

function createWindow () {
  // Create the browser window.
  win = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: ''
    , icon: `${__dirname}/dist/leaf128.png` 
    })

  // and load the index.html of the app.
  win.loadURL(`file://${__dirname}/dist/index.html`)


  // Emitted when the window is closed.
  win.on('closed', () => {
    // Dereference the window object, usually you would store windows
    // in an array if your app supports multi windows, this is the time
    // when you should delete the corresponding element.
    win = null
  })
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

const menuTemplate = 
  [ { label: 'File'
    , submenu:
        [ { label: 'Commit Changes'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.send('commit-changes')
            }
          }
        , { label: 'Save As JSON'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.send('save-as-json')
            }
          }
        , { label: 'Save As Markdown'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.send('save-as-markdown')
            }
          }
        ]
    }
  , { label: 'Debug'
    , submenu: 
        [ { label: 'Reload'
          , accelerator: 'F5'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.reload()
            }
          }
        , { label: 'Show Dev Tools'
          , accelerator: process.platform === 'darwin' ? 'Alt+Command+I' : 'Ctrl+Shift+I'
          , click (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.toggleDevTools()
            }
          }
        ]
    }
  ]
const menu = Menu.buildFromTemplate(menuTemplate)
Menu.setApplicationMenu(menu)

ipcMain.on('save-as-json', (event, arg) => {
  saveAsJSON(arg)
})

ipcMain.on('save-as-markdown', (event, arg) => {
  saveAsMarkdown(arg)
})

saveAsJSON = function(json){
  dialog.showSaveDialog({title: 'Save as JSON', defaultPath: __dirname }, function(e){
    fs.writeFile(e, JSON.stringify(json, null, 2), function (err) { if(err) { console.log(err.message)}})
  })
}
saveAsMarkdown = function(md){
  dialog.showSaveDialog({title: 'Save as JSON', defaultPath: __dirname }, function(e){
    fs.writeFile(e, md,function (err) { if(err) { console.log(err.message)}})
  })
}
