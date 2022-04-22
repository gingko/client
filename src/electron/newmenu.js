function getHomeMenuTemplate (handlers) {
  return getDocMenuTemplate(handlers)
}

function getDocMenuTemplate (handlers, isUntitled) {
  return [
    {
      label: 'File',
      role: 'fileMenu',
      submenu:
      [{
        label: 'New File',
        accelerator: 'CommandOrControl+N',
        click: handlers.clickedNew
      },
      {
        label: 'Open File',
        accelerator: 'CommandOrControl+O',
        click: handlers.clickedOpen
      },
      {
        label: isUntitled ? 'Save' : 'Saved',
        accelerator: 'CommandOrControl+S',
        click: handlers.clickedSaveAs,
        enabled: isUntitled
      },
      {
        label: 'Save As...',
        accelerator: 'CommandOrControl+Shift+S',
        click: handlers.clickedSaveAs
      }
      ]
    },
    {
      label: 'Toggle Dev Tools',
      role: 'toggleDevTools'
    }
  ]
}

module.exports =
  { getDocMenuTemplate, getHomeMenuTemplate }
