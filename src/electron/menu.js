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


  return [ { label: 'File'
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

module.exports =
  { menuFunction: menuFunction
  }
