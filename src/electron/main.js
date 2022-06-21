import * as fs from 'fs/promises'
import { app, BrowserWindow, ipcMain, dialog, Menu, shell } from 'electron'
import { autoUpdater } from 'electron-updater'
import { getHomeMenuTemplate, getDocMenuTemplate } from './menu'
import commitTree from './commit'
import pandoc from './pandoc'
import filenamifyPath from 'filenamify'
import _ from 'lodash'
import { Elm } from '../elm/Electron/Worker'
const config = require('../../config.js')
const log = require('electron-log')
const path = require('path')
const crypto = require('crypto')
const Store = require('electron-store')
const levelup = require('levelup')
const leveldown = require('leveldown')

// Setup file-based logging
autoUpdater.logger = log
autoUpdater.logger.transports.file.level = 'info'
log.info('App starting...')

// Handle error messages and show dialog to user
const unhandled = require('electron-unhandled')
unhandled({ showDialog: true, logger: log.functions.error })

const docWindows = new Map()
const globalStore = new Store({ accessPropertiesByDotNotation: false })
const hiddenStore = new Store({ name: 'kernel', encryptionKey: '79df64f73eab9bc0d7b448d2008d876e' })
let recentDocuments
let elmWorker

const createHomeWindow = async () => {
  const handlers = {
    clickedNew: (item, focusedWindow) => clickedNew(focusedWindow),
    clickedOpen: (item, focusedWindow) => clickedOpen(focusedWindow, true),
    clickedRecentDoc: (item, focusedWindow, openPath) => openDocument(focusedWindow, openPath, true),
    clickedExit: (item, focusedWindow) => app.quit(),
    clickedShowShortcuts: (item, focusedWindow) => clickedShowShortcuts(focusedWindow),
    clickedHelpVideos: (item, focusedWindow) => clickedHelpVideos(focusedWindow),
    clickedFAQ: (item, focusedWindow) => clickedFAQ(focusedWindow),
    clickedContactSupport: (item, focusedWindow) => clickedContactSupport(focusedWindow)
  }
  recentDocuments = globalStore.get('recentDocuments', [])
  const template = getHomeMenuTemplate(handlers, isMac(), recentDocuments, app.getName())
  const menu = Menu.buildFromTemplate(template)
  Menu.setApplicationMenu(menu)

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
          { name: 'Markdown Document', extensions: ['md'] },
          { name: 'All Files', extensions: ['*'] }
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
  const winData = docWindows.get(win)
  const origPath = winData.filePath
  const defaultPath = origPath.startsWith(app.getPath('temp')) ? app.getPath('documents') : origPath

  const { filePath, canceled } = await dialog.showSaveDialog(win, {
    defaultPath,
    filters:
        [
          { name: 'Gingko Writer Document', extensions: ['gkw'] },
          { name: 'Markdown Document', extensions: ['md'] },
          { name: 'All Files', extensions: ['*'] }
        ]
  }
  )
  if (!canceled && filePath) {
    // Set new filePath
    winData.filePath = filePath
    winData.isUntitled = false

    // Copy Untitled to new location
    await fs.copyFile(origPath, filePath)

    // Create Swap file copy
    await fs.copyFile(origPath, filePath + '.swp')

    // Open swapFileHandle for new file
    winData.swapFileHandle = await fs.open(filePath, 'r+')
    await addToRecentDocuments(filePath)

    // Close and copy undoDb, delete original
    await winData.undoDb.close()
    await fs.cp(getUndoPath(origPath), getUndoPath(filePath), { recursive: true })
    await fs.rm(getUndoPath(origPath), { recursive: true, force: true })
    winData.undoDb = levelup(leveldown(getUndoPath(filePath)))

    // Save new winData
    docWindows.set(win, winData)

    // Send filePath to renderer and Elm
    await win.webContents.send('file-saved', [filePath, Date.now(), winData.isUntitled])

    // Set doc window menu & titlebar
    win.setTitle(getTitleText(winData))
    const menu = Menu.buildFromTemplate(getDocMenuTemplate(handlers, false, isMac(), recentDocuments, app.getName(), false))
    Menu.setApplicationMenu(menu)
  }
}

async function clickedShowShortcuts (win) {
  const shortcutsWin = new BrowserWindow({
    parent: win,
    width: 1000,
    height: 800,
    backgroundColor: 'hsl(202deg 62% 92%)'
  })

  shortcutsWin.menuBarVisible = false

  await shortcutsWin.loadFile(path.join(__dirname, '/static/shortcuts-modal.html'))
}

async function clickedHelpVideos (win) {
  const helpVideoModal = new BrowserWindow({
    parent: win,
    modal: !isMac(),
    width: 800,
    height: 400,
    resizable: false,
    backgroundColor: 'hsl(202, 34%, 96%)'
  })

  helpVideoModal.menuBarVisible = false

  await helpVideoModal.loadFile(path.join(__dirname, '/static/videos-modal.html'))
}

async function clickedFAQ (win) {
  const faqModal = new BrowserWindow({
    parent: win,
    modal: !isMac(),
    width: 445,
    height: 600,
    show: false,
    resizable: false,
    backgroundColor: 'hsl(202, 34%, 96%)'
  })

  faqModal.menuBarVisible = false

  await faqModal.loadFile(path.join(__dirname, '/static/faq-modal.html'))
  setTimeout(() => { faqModal.show() }, 400)
}

async function clickedContactSupport (win) {
  const supportModal = new BrowserWindow({
    parent: win,
    modal: !isMac(),
    width: 800,
    height: 445,
    backgroundColor: 'white',
    webPreferences: {
      nodeIntegration: true,
      contextIsolation: false
    }
  })

  supportModal.menuBarVisible = false

  await supportModal.loadFile(path.join(__dirname, '/static/support-modal.html'))
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
  const winData = docWindows.get(win)

  // This may miss some config events, but it prevents slowing down the app
  // every time the active card is changed.
  debouncedLocalStoreSet(winData.filePath, key, val)
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
  const winData = docWindows.get(win)

  const origPath = winData.filePath.replace('.gkw', '.' + data[0])
  const defaultPath = origPath.startsWith(app.getPath('temp')) ? app.getPath('documents') : origPath

  const { filePath, canceled } = await dialog.showSaveDialog(win, { defaultPath, filters: [{ name: data[0], extensions: [data[0]] }] })
  if (!canceled && filePath) {
    if (data[0] === 'docx') {
      try {
        await pandoc(filePath, data[1])
      } catch (e) {
        await dialog.showMessageBox(win, { title: 'Failed to Export', message: e.toString() })
      }
    } else {
      await fs.writeFile(filePath, data[1])
    }
  }
})

ipcMain.on('save-file', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)
  const winData = docWindows.get(win)

  const filePath = winData.filePath
  try {
    const { bytesWritten } = await winData.swapFileHandle.write(data[1], 0)
    await winData.swapFileHandle.truncate(bytesWritten)
    await fs.copyFile(filePath + '.swp', filePath)
    await webContents.send('file-saved', [filePath, Date.now(), winData.isUntitled])
    win.setTitle(getTitleText(winData))
  } catch (e) {
    console.error(e)
  }
})

ipcMain.on('edit-mode-changed', (event, isEditMode) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)
  const winData = docWindows.get(win)

  const menu = Menu.buildFromTemplate(getDocMenuTemplate(handlers, winData.isUntitled, isMac(), recentDocuments, app.getName(), isEditMode))
  Menu.setApplicationMenu(menu)
})

ipcMain.on('commit-data', async (event, commitData) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)
  const winData = docWindows.get(win)

  const [commitSha, objects] = await commitTree(commitData.author, commitData.parents, commitData.workingTree, Date.now(), commitData.metadata)
  objects.push({ _id: 'heads/master', type: 'ref', value: commitSha, ancestors: [], _rev: '' })

  const ops = objects
    .filter((o) => !winData.savedImmutables.has(o._id))
    .map((o) => {
      const objId = o._id
      return { type: 'put', key: objId, value: JSON.stringify(_.omit(o, ['_id'])) }
    })
  try {
    await winData.undoDb.batch(ops)
    ops.forEach((o) => {
      if (o.key !== 'heads/master') winData.savedImmutables.add(o.key)
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

app.on('open-file', (event, filePath) => {
  createDocWindow(filePath)
})

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
    await createHomeWindow()
  }

  // Check Trial and Serial state
  let firstRun = hiddenStore.get('firstRun', 'not-set')
  if (firstRun === 'not-set') {
    firstRun = Date.now()
    hiddenStore.set('firstRun', firstRun)
  }
  const email = globalStore.get('email', '')
  const storedSerial = globalStore.get('serial', '')

  if (!validSerial(email, storedSerial)) {
    const limit = 30 // 30 days for now
    const daysUsed = Math.round((Date.now() - Number(firstRun)) / (1000 * 3600 * 24))
    const trialDisplayDays = [7, 9, 11, 12, 13, 14]

    if (trialDisplayDays.includes(daysUsed)) {
      const firstWindow = BrowserWindow.getAllWindows()[0]
      createTrialWindow(firstWindow, daysUsed, limit)
    }
  }

  // Initialize Elm Worker to reuse Elm code "server-side"
  elmWorker = Elm.Electron.Worker.init({ flags: Date.now() })
  elmWorker.ports.fromElm.subscribe((elmData) => {
    switch (elmData[0]) {
      case 'ImportDone':
        createDocWindow(null, elmData[1].data)
        break

      case 'ImportError':
        dialog.showErrorBox('Import Error', elmData[1])
        break
    }
  })

  // Initialize auto-update
  autoUpdater.checkForUpdatesAndNotify()

  app.on('activate', () => {
    // On macOS re-create a window when the dock icon is clicked
    // and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createHomeWindow()
  })
})

app.on('web-contents-created', (event, webContents) => {
  webContents.on('will-navigate', async (event, url) => {
    switch (url) {
      case config.DESKTOP_PURCHASE_URL:
        break

      case config.DESKTOP_PURCHASE_SUCCESS_URL:
        await BrowserWindow.fromWebContents(webContents).loadFile(path.join(__dirname, '/static/trial-success-modal.html'))
        break

      case 'electron://enter-license-modal':
        await BrowserWindow.fromWebContents(webContents).loadFile(path.join(__dirname, '/static/enter-license-modal.html'))
        break

      default:
        event.preventDefault()
        await shell.openExternal(url)
    }
  })
})

// Quit when all windows are closed, except on macOS.
app.on('window-all-closed', () => {
  if (!isMac()) app.quit()
})

/* ==== Doc Window ==== */

const handlers =
  {
    clickedNew: async () => await clickedNew(false),
    clickedOpen: async (item, focusedWindow) => await clickedOpen(focusedWindow),
    clickedRecentDoc: (item, focusedWindow, openPath) => openDocument(focusedWindow, openPath, false),
    clickedSaveAs: async (item, focusedWindow) => await saveThisAs(focusedWindow),
    clickedExport: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-export'),
    clickedClose: (item, focusedWindow) => focusedWindow.close(),
    clickedExit: (item, focusedWindow) => app.quit(),
    clickedUndo: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-undo'),
    clickedCut: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-cut'),
    clickedCopy: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-copy'),
    clickedPaste: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-paste'),
    clickedPasteInto: async (item, focusedWindow) => await focusedWindow.webContents.send('clicked-paste-into'),
    clickedShowShortcuts: (item, focusedWindow) => clickedShowShortcuts(focusedWindow),
    clickedHelpVideos: (item, focusedWindow) => clickedHelpVideos(focusedWindow),
    clickedFAQ: (item, focusedWindow) => clickedFAQ(focusedWindow),
    clickedContactSupport: (item, focusedWindow) => clickedContactSupport(focusedWindow)
  }
async function createDocWindow (filePath, initFileData) {
  for (const winData of docWindows.values()) {
    if (winData.filePath === filePath) {
      await dialog.showMessageBox({ title: 'File already open', message: 'File already open', detail: 'Cannot open file twice' })
      return
    }
  }

  const isUntitled = filePath === null
  recentDocuments = globalStore.get('recentDocuments', [])

  const template = Menu.buildFromTemplate(getDocMenuTemplate(handlers, isUntitled, isMac(), recentDocuments, app.getName(), isUntitled))
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
  const winData = { filePath, swapFileHandle, isUntitled, undoDb, savedImmutables: new Set() }
  docWindows.set(docWin, winData)

  // Get localStore data if exists
  const fileSettings = globalStore.get(filePath, null)

  // Initialize Renderer
  docWin.setTitle(getTitleText(winData))
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
  docWin.on('closed', async (evt) => {
    const winData = docWindows.get(docWin)
    await winData.swapFileHandle.close()
    await winData.undoDb.close()
    await fs.unlink(winData.filePath + '.swp')
    docWindows.delete(docWin)
  })
}

async function createTrialWindow (win, daysUsed, limit) {
  const trialWin = new BrowserWindow({
    width: 600,
    height: 450,
    parent: win,
    modal: true,
    webPreferences: {
      preload: path.join(__dirname, 'trial-preload.js'),
      additionalArguments: ['trialarg=' + String(daysUsed), 'trialarg=' + String(limit), 'trialarg=' + config.DESKTOP_PURCHASE_URL]
    }
  })

  trialWin.menuBarVisible = false
  await trialWin.loadFile(path.join(__dirname, '/static/trial-modal.html'))
  trialWin.focus()
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
  const namifiedPath = filenamifyPath(filePath, { replacement: '%' })
  return path.join(app.getPath('userData'), 'versionhistory', namifiedPath)
}

async function addToRecentDocuments (filePath) {
  app.addRecentDocument(filePath)
  const { birthtimeMs, mtimeMs } = await fs.stat(filePath)
  const atimeMs = Date.now()
  const docEntry = { name: path.basename(filePath, '.gkw'), path: filePath, birthtimeMs, atimeMs, mtimeMs }
  recentDocuments = recentDocuments.filter(rd => rd.path !== filePath).concat(docEntry)
  globalStore.set('recentDocuments', recentDocuments)
}

async function removeFromRecentDocuments (filePath) {
  const newRecentDocs = recentDocuments.filter(rd => rd.path !== filePath)
  app.clearRecentDocuments()
  newRecentDocs.map(rd => app.addRecentDocument(rd.path))
  recentDocuments = newRecentDocs
  globalStore.set('recentDocuments', newRecentDocs)
}

function isMac () {
  return process.platform === 'darwin'
}

/* ==== Validation & Payment ==== */
function validSerial (email, storedSerial) {
  // I've decided against complicated anti-piracy checks.
  // Instead, I want things as easy as possible for the user, while still being able to make a living.
  //
  // If you really can't afford Gingko, even after allowing for discounts, please get in touch with me first.
  const sha256 = crypto.createHash('sha256')
  const hash = sha256.update(email + config.DESKTOP_SERIAL_SALT).digest('hex')
  const serial = [
    hash.substr(4, 4),
    hash.substr(12, 4),
    hash.substr(20, 4),
    hash.substr(28, 4)
  ]
    .join('-')
    .toUpperCase()

  return storedSerial === serial
}
