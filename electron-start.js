const {app, BrowserWindow, dialog, Menu, ipcMain} = require('electron')
const fs = require('fs')

// Keep a global reference of the window object, if you don't, the window will
// be closed automatically when the JavaScript object is garbage collected.
let win
let currentFile = null

function createWindow () {
  // Create the browser window.
  win = new BrowserWindow(
    { width: 800
    , height: 600
    , backgroundColor: '#32596b'
    , icon: `${__dirname}/dist/leaf128.png` 
    })

  // and load the index.html of the app.
  win.loadURL(`file://${__dirname}/dist/index.html`)

  win.on('close', (e) => {
    var options = 
      { title: "test"
      , message: "Save changes before closing?"
      , buttons: ["Close Without Saving", "Cancel", "Save"]
      , defaultId: 2
      }
    var choice = dialog.showMessageBox(options)

    switch (choice) {
      case 1:
        e.preventDefault()
        break
      case 2:
        win.webContents.send('save-and-close')
        e.preventDefault()
        break
    }
  })

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
        [ { label: 'Open File...'
          , click (item, focusedWindow) {
              dialog.showOpenDialog(focusedWindow, {title: "Open File...", defaultPath: __dirname, properties: ['openFile']}, function(e) {
                fs.readFile(e[0], (err, data) => {
                  if (err) throw err;
                  currentFile = e[0]
                  focusedWindow.webContents.send('file-read', e[0], data)
                })
              })
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

ipcMain.on('save', (event, arg) => {
  saveModel(arg)
})

ipcMain.on('save-and-close', (event, arg) => {
  saveModel(arg)

  app.exit(0)
})

ipcMain.on('save-as', (event, arg) => {
  saveModelAs(arg)
})

ipcMain.on('save-as-json', (event, arg) => {
  saveAsJSON(arg)
})

ipcMain.on('save-as-markdown', (event, arg) => {
  saveAsMarkdown(arg)
})

saveModel = function(model){
  if (currentFile) {
    fs.writeFile(currentFile, JSON.stringify(model, null, 2),function (err) { 
      if(err) { 
        console.log(err.message)
      }
    })
  } else {
    saveModelAs(model)
  }
}
saveModelAs = function(model){
  dialog.showSaveDialog({title: 'Save As', defaultPath: __dirname }, function(e){
    fs.writeFile(e, JSON.stringify(model, null, 2),function (err) { if(err) { console.log(err.message)}})
  })
}
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
