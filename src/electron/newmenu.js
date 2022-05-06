function getHomeMenuTemplate (handlers, isMac, recentDocs) {
  const docMenuTemplate = getDocMenuTemplate(handlers, true, isMac, recentDocs)
  docMenuTemplate[0].submenu = docMenuTemplate[0].submenu.slice(0, 3)
  return docMenuTemplate
}

function getDocMenuTemplate (handlers, isUntitled, isMac, recentDocs) {
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
  console.log(recentDocsMenu)
  return [
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
      }
      ]
    }
  ]
}

module.exports =
  { getDocMenuTemplate, getHomeMenuTemplate }
