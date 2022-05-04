import * as fs from 'fs/promises'
import { app, BrowserWindow, ipcMain, dialog, Menu } from 'electron'
import { getHomeMenuTemplate, getDocMenuTemplate } from './newmenu'
import commitTree from './commit'
import pandoc from './pandoc'
import _ from 'lodash'
import { Elm } from '../elm/Worker'
const path = require('path')
const crypto = require('crypto')
const Store = require('electron-store')
const levelup = require('levelup')
const leveldown = require('leveldown')

const docWindows = {}
const globalStore = new Store({ accessPropertiesByDotNotation: false })
let elmWorker

const createHomeWindow = async () => {
  const handlers = {
    clickedNew: (item, focusedWindow) => clickedNew(focusedWindow),
    clickedOpen: (item, focusedWindow) => clickedOpen(focusedWindow, true),
    clickedSaveAs: async (item, focusedWindow) => await saveThisAs(focusedWindow)
  }
  const template = Menu.buildFromTemplate(getHomeMenuTemplate(handlers))
  Menu.setApplicationMenu(template)

  const homeWin = new BrowserWindow({
    width: 800,
    height: 600,
    show: false,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  homeWin.setTitle('Gingko Writer - Home')
  await homeWin.loadFile(path.join(__dirname, '/static/home.html'))

  // Set recent docs & language
  const recentDocuments = globalStore.get('recentDocuments', [])
  const language = globalStore.get('language', 'en')
  await homeWin.webContents.send('file-received', { recentDocuments, language })

  homeWin.once('ready-to-show', () => {
    homeWin.show()
  })
}

/* ==== shared handlers ==== */

async function clickedNew (win) {
  const fromHomePage = !!win
  await openDocument(win, null, fromHomePage)
}

async function clickedOpen (win, fromHomePage) {
  const dialogReturnValue = await dialog.showOpenDialog(win,
    {
      properties: ['openFile'],
      defaultPath: app.getPath('documents'),
      filters:
        [{ name: 'Gingko Writer Document', extensions: ['gkw'] },
          { name: 'Gingko Desktop Legacy', extensions: ['gko'] },
          { name: 'Markdown Document', extensions: ['md'] }
        ]
    })

  if (dialogReturnValue.filePaths.length !== 0) {
    await openDocument(win, dialogReturnValue.filePaths[0], fromHomePage)
  }
}

async function clickedImport (win, fromHomePage) {
  const dialogReturnValue = await dialog.showOpenDialog(win,
    {
      properties: ['openFile'],
      defaultPath: app.getPath('documents'),
      filters:
        [{ name: 'JSON', extensions: ['json'] }
        ]
    })

  if (dialogReturnValue.filePaths.length !== 0) {
    const filePath = dialogReturnValue.filePaths[0]
    const fileData = await fs.readFile(filePath, { encoding: 'utf8' })
    elmWorker.ports.toElm.send([fileData, fromHomePage])
  }
}

async function openDocument (win, docPath, fromHomePage) {
  if (fromHomePage) win.hide()
  await createDocWindow(docPath)
  if (fromHomePage) win.destroy()
}

async function saveThisAs (win) {
  const origPath = docWindows[win.id].filePath
  const defaultPath = origPath.startsWith(app.getPath('temp')) ? app.getPath('documents') : origPath

  const { filePath, canceled } = await dialog.showSaveDialog(win, { defaultPath })
  if (!canceled && filePath) {
    // Set new filePath
    docWindows[win.id].filePath = filePath

    // Copy Untitled to new location
    await fs.copyFile(origPath, filePath)

    // Create Swap file copy
    await fs.copyFile(origPath, filePath + '.swp')

    // Open swapFileHandle for new file
    docWindows[win.id].swapFileHandle = await fs.open(filePath, 'r+')
    await addToRecentDocuments(filePath)

    // Close and copy undoDb, delete original
    await docWindows[win.id].undoDb.close()
    await fs.cp(getUndoPath(origPath), getUndoPath(filePath), { recursive: true })
    await fs.rm(getUndoPath(origPath), { recursive: true, force: true })
    docWindows[win.id].undoDb = levelup(leveldown(getUndoPath(filePath)))

    await win.webContents.send('file-saved', filePath)
    win.setTitle(getTitleText(docWindows[win.id]))
    const template = Menu.buildFromTemplate(getDocMenuTemplate(handlers, false))
    Menu.setApplicationMenu(template)
  }
}

/* ==== IPC handlers ==== */

function localStoreSet (filePath, key, val) {
  const objToSet = {}
  objToSet[key] = val
  globalStore.set(filePath, objToSet)
}
const debouncedLocalStoreSet = _.debounce(localStoreSet, 827, { leading: true, trailing: true })

ipcMain.on('local-store-set', (event, key, val) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  // This may miss some config events, but it prevents slowing down the app
  // every time the active card is changed.
  debouncedLocalStoreSet(docWindows[win.id].filePath, key, val)
})

ipcMain.on('clicked-new', (event) => {
  const webContents = event.sender
  const homeWindow = BrowserWindow.fromWebContents(webContents)
  clickedNew(homeWindow)
})

ipcMain.on('clicked-open', (event) => {
  const webContents = event.sender
  const homeWindow = BrowserWindow.fromWebContents(webContents)
  clickedOpen(homeWindow, true)
})

ipcMain.on('clicked-import', (event) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)
  clickedImport(win, true)
})

ipcMain.on('clicked-document', (event, docPath) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)
  openDocument(win, docPath, true)
})

ipcMain.on('clicked-remove-document', (event, docPath) => {
  removeFromRecentDocuments(docPath)
})

ipcMain.on('export-file', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const origPath = docWindows[win.id].filePath.replace('.gkw', '.' + data[0])
  const defaultPath = origPath.startsWith(app.getPath('temp')) ? app.getPath('documents') : origPath

  const { filePath, canceled } = await dialog.showSaveDialog(win, { defaultPath, filters: [{ name: data[0], extensions: [data[0]] }] })
  if (!canceled && filePath) {
    if (data[0] === 'docx') {
      try {
        await pandoc(filePath, data[1])
      } catch (e) {
        console.error(e)
      }
    } else {
      await fs.writeFile(filePath, data[1])
    }
  }
})

ipcMain.on('save-file', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const filePath = docWindows[win.id].filePath
  try {
    const { bytesWritten } = await docWindows[win.id].swapFileHandle.write(data[1], 0)
    await docWindows[win.id].swapFileHandle.truncate(bytesWritten)
    await fs.copyFile(filePath + '.swp', filePath)
    await webContents.send('file-saved', [filePath, Date.now()])
    win.setTitle(getTitleText(docWindows[win.id]))
  } catch (e) {
    console.error(e)
  }
})

ipcMain.on('commit-data', async (event, commitData) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const [commitSha, objects] = await commitTree(commitData.author, commitData.parents, commitData.workingTree, Date.now(), commitData.metadata)
  objects.push({ _id: 'heads/master', type: 'ref', value: commitSha, ancestors: [], _rev: '' })

  const ops = objects
    .filter((o) => !docWindows[win.id].savedImmutables.has(o._id))
    .map((o) => {
      const objId = o._id
      return { type: 'put', key: objId, value: JSON.stringify(_.omit(o, ['_id'])) }
    })
  try {
    await docWindows[win.id].undoDb.batch(ops)
    ops.forEach((o) => {
      if (o.key !== 'heads/master') docWindows[win.id].savedImmutables.add(o.key)
    })
    await webContents.send('commit-data-result', objectsToElmData(objects))
  } catch (e) {
    console.error(e)
  }
})

ipcMain.on('maybe-close-window', async (event) => {
  const win = BrowserWindow.fromWebContents(event.sender)
  const { response } = await dialog.showMessageBox(win, {
    type: 'question',
    message: 'Do you want to save your changes?',
    title: 'Save changes?',
    buttons: ["Don't Save", 'Cancel', 'Save']
  })

  switch (response) {
    case 0:
      setTimeout(() => { win.destroy() }, 0)
      break

    case 2:
      await saveThisAs(win)
      win.destroy()
      break
  }
})

ipcMain.on('close-window', (event) => {
  BrowserWindow.fromWebContents(event.sender).close()
})

/* ==== App Initialization ==== */

app.whenReady().then(async () => {
  let pathArgument
  if (process.defaultApp && typeof process.argv[2] === 'string') {
    try {
      await fs.access(process.argv[2])
      pathArgument = process.argv[2]
    } catch (e) {
      pathArgument = false
    }
  } else if (!process.defaultApp && typeof process.argv[1] === 'string') {
    try {
      await fs.access(process.argv[1])
      pathArgument = process.argv[1]
    } catch (e) {
      pathArgument = false
    }
  }
  if (pathArgument) {
    await createDocWindow(pathArgument)
  } else {
    createHomeWindow()
  }

  // Initialize Elm Worker to reuse Elm code "server-side"
  elmWorker = Elm.Worker.init({ flags: Date.now() })
  elmWorker.ports.fromElm.subscribe((elmData) => {
    switch (elmData[0]) {
      case 'ImportDone':
        console.log('elmData', elmData)
        createDocWindow(null, elmData[1].data)
        break
    }
  })

  app.on('activate', () => {
    // On macOS re-create a window when the dock icon is clicked
    // and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createHomeWindow()
  })
})

// Quit when all windows are closed, except on macOS.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})

/* ==== Doc Window ==== */

const handlers =
  {
    clickedNew: async () => await clickedNew(false),
    clickedOpen: async (item, focusedWindow) => await clickedOpen(focusedWindow),
    clickedSaveAs: async (item, focusedWindow) => await saveThisAs(focusedWindow),
    clickedExport: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-export')
  }
async function createDocWindow (filePath, initFileData) {
  for (const winId in docWindows) {
    if (docWindows[winId].filePath === filePath) {
      await dialog.showMessageBox({ title: 'File already open', message: 'File already open', detail: 'Cannot open file twice' })
      return
    }
  }

  const isUntitled = filePath === null

  const template = Menu.buildFromTemplate(getDocMenuTemplate(handlers, isUntitled))
  Menu.setApplicationMenu(template)

  const docWin = new BrowserWindow({
    width: 800,
    height: 600,
    backgroundColor: 'hsl(202, 72%, 41%)',
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let fileData = null
  let swapFileHandle
  const d = new Date()
  const sha1Hash = crypto.createHash('sha1')
  const fileHash = sha1Hash.update(d.getTime() + '').digest('hex').slice(0, 6)
  const dateString = d.toISOString().slice(0, 10)
  if (isUntitled) {
    // Initialize New Document
    filePath = path.join(app.getPath('temp'), `Untitled-${dateString}-${fileHash}.gkw`)
    swapFileHandle = await fs.open(filePath + '.swp', 'w')
    if (initFileData) fileData = initFileData
  } else {
    // Save backup copy
    await fs.copyFile(filePath, path.join(app.getPath('temp'), `${path.basename(filePath, '.gkw')}-${dateString}-${fileHash}.gkw.bak`))

    // Open swap copy
    await fs.copyFile(filePath, filePath + '.swp')

    // Load Document
    swapFileHandle = await fs.open(filePath + '.swp', 'r+')
    fileData = await swapFileHandle.readFile({ encoding: 'utf8' })

    // Add to Recent documents
    await addToRecentDocuments(filePath)
  }

  // Initialize undo data
  const undoPath = getUndoPath(filePath)
  await fs.mkdir(undoPath, { recursive: true })
  const undoDb = levelup(leveldown(undoPath))

  const undoData = []
  for await (const [key, value] of undoDb.iterator()) {
    try {
      const val = JSON.parse(value.toString())
      const newData = Object.assign({ _id: key.toString() }, val)
      undoData.push(newData)
    } catch (e) {
      console.log(e)
    }
  }
  const newUndoData = objectsToElmData(undoData)

  // Save window-specific data
  docWindows[docWin.id] = { filePath, swapFileHandle, undoDb, savedImmutables: new Set() }

  // Get localStore data if exists
  const fileSettings = globalStore.get(filePath, null)

  // Initialize Renderer
  docWin.setTitle(getTitleText(docWindows[docWin.id]))
  await docWin.loadFile(path.join(__dirname, '/static/renderer.html'))
  await docWin.webContents.send('file-received',
    {
      filePath,
      fileData,
      fileSettings,
      undoData: newUndoData,
      isUntitled
    }
  )

  // Prevent title being set from HTML
  docWin.on('page-title-updated', (evt) => {
    evt.preventDefault()
  })

  // Handle closing
  docWin.on('close', async (evt) => {
    await docWindows[docWin.id].swapFileHandle.close()
    await fs.unlink(docWindows[docWin.id].filePath + '.swp')
    delete docWindows[docWin.id]
  })
}

/* ==== Helper Functions */

function getTitleText (windowInfo) {
  return `${path.basename(windowInfo.filePath)} - Gingko Writer`
}

function objectsToElmData (objs) {
  const groupFn = (r) => (Object.prototype.hasOwnProperty.call(r, 'type') ? r.type : r._id)
  return _.groupBy(objs, groupFn)
}

function getUndoPath (filePath) {
  return path.join(app.getPath('userData'), 'versionhistory', filePath.split(path.sep).join('%'))
}

async function addToRecentDocuments (filePath) {
  app.addRecentDocument(filePath)
  const recentDocs = globalStore.get('recentDocuments', [])
  const { birthtimeMs, mtimeMs } = await fs.stat(filePath)
  const atimeMs = Date.now()
  const docEntry = { name: path.basename(filePath, '.gkw'), path: filePath, birthtimeMs, atimeMs, mtimeMs }
  globalStore.set('recentDocuments', recentDocs.filter(rd => rd.path !== filePath).concat(docEntry))
}

async function removeFromRecentDocuments (filePath) {
  const newRecentDocs = globalStore.get('recentDocuments', []).filter(rd => rd.path !== filePath)
  app.clearRecentDocuments()
  newRecentDocs.map(rd => app.addRecentDocument(rd.path))
  globalStore.set('recentDocuments', newRecentDocs)
}
