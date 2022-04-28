import * as fs from 'fs/promises'
import { app, BrowserWindow, ipcMain, dialog, Menu } from 'electron'
import { getHomeMenuTemplate, getDocMenuTemplate } from './newmenu'
import commitTree from './commit'
import pandoc from './pandoc'
import _ from 'lodash'
const path = require('path')
const crypto = require('crypto')
const Store = require('electron-store')
const levelup = require('levelup')
const leveldown = require('leveldown')

const docWindows = {}
const globalStore = new Store()

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

  // Set recent docs
  await homeWin.webContents.send('file-received', { recentDocuments: [{ name: 'File Name', path: '/home/adri/test/safdf.gkw', birthtimeMs: 12345, atimeMs: 12345, mtimeMs: 12345 }] })

  homeWin.once('ready-to-show', () => {
    homeWin.show()
  })
}

/* ==== shared handlers ==== */

async function clickedNew (win) {
  if (win) win.hide()
  await createDocWindow(null)
  if (win) win.destroy()
}

async function clickedOpen (win, close) {
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
    if (close) win.hide()
    await createDocWindow(dialogReturnValue.filePaths[0])
    if (close) win.destroy()
  }
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

    // Open filehandle for new file
    docWindows[win.id].filehandle = await fs.open(filePath, 'r+')

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
  const { bytesWritten } = await docWindows[win.id].filehandle.write(data[1], 0)
  await docWindows[win.id].filehandle.truncate(bytesWritten)
  await webContents.send('file-saved', [filePath, Date.now()])
  win.setTitle(getTitleText(docWindows[win.id]))
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

/* ==== Initialization ==== */

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
async function createDocWindow (filePath) {
  for (const winId in docWindows) {
    if (docWindows[winId].filePath === filePath) {
      await dialog.showMessageBox({ title: 'File already open', message: 'File already open', detail: 'Cannot open file twice' })
      return
    }
  }

  const template = Menu.buildFromTemplate(getDocMenuTemplate(handlers, filePath === null))
  Menu.setApplicationMenu(template)

  const docWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let fileData = null
  let filehandle
  const d = new Date()
  const sha1Hash = crypto.createHash('sha1')
  const fileHash = sha1Hash.update(d.getTime() + '').digest('hex').slice(0, 6)
  const dateString = d.toISOString().slice(0, 10)
  if (filePath == null) {
    // Initialize New Document
    filePath = path.join(app.getPath('temp'), `Untitled-${dateString}-${fileHash}.gkw`)
    filehandle = await fs.open(filePath, 'w')
  } else {
    // Save backup copy
    await fs.copyFile(filePath, path.join(app.getPath('temp'), `${path.basename(filePath, '.gkw')}-${dateString}-${fileHash}.gkw.bak`))

    // Load Document
    filehandle = await fs.open(filePath, 'r+')
    fileData = await filehandle.readFile({ encoding: 'utf8' })

    // Add to Recent documents
    app.addRecentDocument(filePath)
    const recentDocs = globalStore.get('recentDocuments', [])
    const { birthtimeMs, atimeMs, mtimeMs } = await filehandle.stat()
    const docEntry = { name: path.basename(filePath, '.gkw'), path: filePath, birthtimeMs, atimeMs, mtimeMs }
    globalStore.set('recentDocuments', recentDocs.filter(rd => rd.path !== filePath).concat(docEntry))
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
  docWindows[docWin.id] = { filePath, filehandle, undoDb, savedImmutables: new Set() }

  // Initialize Renderer
  docWin.setTitle(getTitleText(docWindows[docWin.id]))
  await docWin.loadFile(path.join(__dirname, '/static/renderer.html'))
  await docWin.webContents.send('file-received', { filePath: filePath, fileData: fileData, undoData: newUndoData })

  // Prevent title being set from HTML
  docWin.on('page-title-updated', (evt) => {
    evt.preventDefault()
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
