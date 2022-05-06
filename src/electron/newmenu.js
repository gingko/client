function getHomeMenuTemplate (handlers, isMac, recentDocs, appName) {
  const docMenuTemplate = getDocMenuTemplate(handlers, true, isMac, recentDocs, appName)
  const fileMenuIdx = isMac ? 1 : 0
  const newFileSubmenu = isMac
    ? docMenuTemplate[fileMenuIdx].submenu.slice(0, 3)
    : docMenuTemplate[fileMenuIdx].submenu.slice(0, 3).concat(docMenuTemplate[fileMenuIdx].submenu.slice(-2))
  docMenuTemplate[fileMenuIdx].submenu = newFileSubmenu
  return docMenuTemplate
}

function getDocMenuTemplate (handlers, isUntitled, isMac, recentDocs, appName) {
  const recentDocView = function (rd, idx) {
    const clickFn = (item, focusedWindow) => { handlers.clickedRecentDoc(item, focusedWindow, rd.path) }
    return { label: '&' + (idx + 1) + '.  ' + rd.name, click: clickFn }
  }

  let recentDocsMenu
  if (isMac) {
    recentDocsMenu = { label: 'Open &Recent', role: 'recentDocuments' }
  } else {
    recentDocsMenu = { label: 'Open &Recent', submenu: recentDocs.map(recentDocView) }
  }
  const template =
  [
    {
      label: '&File',
      role: 'fileMenu',
      submenu:
      [{
        label: '&New File',
        accelerator: 'CommandOrControl+N',
        click: handlers.clickedNew
      },
      {
        label: '&Open...',
        accelerator: 'CommandOrControl+O',
        click: handlers.clickedOpen
      },
      recentDocsMenu,
      {
        label: '&Close',
        click: handlers.clickedClose
      },
      { type: 'separator' },
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
      },
      { type: 'separator' },
      {
        label: 'Export...',
        click: handlers.clickedExport
      },
      { type: 'separator' },
      {
        label: `E&xit ${appName}`,
        click: handlers.clickedExit
      }
      ]
    }
  ]

  if (isMac) {
    template.unshift(
      {
        label: appName,
        submenu: [
          {
            label: `About ${appName}`,
            role: 'about'
          },
          { type: 'separator' },
          {
            label: 'Services',
            role: 'services',
            submenu: []
          },
          { type: 'separator' },
          {
            label: `Hide ${appName}`,
            accelerator: 'Command+H',
            role: 'hide'
          },
          {
            label: 'Hide Others',
            accelerator: 'Command+Alt+H',
            role: 'hideothers'
          },
          {
            label: 'Show All',
            role: 'unhide'
          },
          { type: 'separator' },
          {
            label: `Quit ${appName}`,
            accelerator: 'Command+Q',
            click: handlers.clickedQuit
          }]
      }
    )
  }

  return template
}

module.exports =
  { getDocMenuTemplate, getHomeMenuTemplate }
