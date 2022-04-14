function getHomeMenuTemplate (handlers) {
  return getDocMenuTemplate(handlers)
}

function getDocMenuTemplate (handlers) {
  return [
    {
      label: 'File',
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
