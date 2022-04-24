import * as fs from 'fs/promises'
import { app, BrowserWindow, ipcMain, dialog, Menu } from 'electron'
import { getHomeMenuTemplate, getDocMenuTemplate } from './newmenu'
import commitTree from './commit'
const path = require('path')
const crypto = require('crypto')
const levelup = require('levelup')
const leveldown = require('leveldown')

const docWindows = {}

const createHomeWindow = () => {
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
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  homeWin.setTitle('Gingko Writer - Home')
  homeWin.loadFile(path.join(__dirname, '/static/home.html'))
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
    docWindows[win.id].filePath = filePath
    await fs.copyFile(origPath, filePath)
    docWindows[win.id].filehandle = await fs.open(filePath, 'r+')
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

ipcMain.on('clicked-open', (event, title) => {
  const webContents = event.sender
  const homeWindow = BrowserWindow.fromWebContents(webContents)
  clickedOpen(homeWindow, true)
})

ipcMain.on('save-file', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const filePath = docWindows[win.id].filePath
  const { bytesWritten } = await docWindows[win.id].filehandle.write(data[1], 0)
  await docWindows[win.id].filehandle.truncate(bytesWritten)
  await webContents.send('file-saved', filePath)
  win.setTitle(getTitleText(docWindows[win.id]))
})

ipcMain.on('commit-data', async (event, commitData) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const [commitSha, objects, filePath] = await commitTree(commitData.author, commitData.parents, commitData.workingTree, Date.now(), commitData.metadata)
  const ops = objects
    .filter((o) => !docWindows[win.id].savedImmutables.has(o._id))
    .map((o) => {
      const objId = o._id
      delete o._id
      return { type: 'put', key: objId, value: JSON.stringify(o) }
    })
  console.log(ops)
  try {
    await docWindows[win.id].undoDb.batch(ops)
    ops.forEach((o) => docWindows[win.id].savedImmutables.add(o.key))
    console.log(docWindows[win.id].savedImmutables)
  } catch (e) {
    console.error(e)
  }
})

ipcMain.on('close-window', (event) => {
  BrowserWindow.fromWebContents(event.sender).close()
})

/* ==== Initialization ==== */

app.whenReady().then(() => {
  createHomeWindow()

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
    clickedSaveAs: async (item, focusedWindow) => await saveThisAs(focusedWindow)
  }
async function createDocWindow (filePath) {
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
  let undoDb
  let undoData = {}
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
    const undoPath = path.join(app.getPath('userData'), 'versionhistory', filePath.split(path.sep).join('%'))
    await fs.mkdir(undoPath, { recursive: true })
    undoDb = levelup(leveldown(undoPath))
    undoDb.createReadStream().on('data', (data) => {
      console.log(data.key.toString(), data.value.toString())
    })
  }
  docWindows[docWin.id] = { filePath, filehandle, undoDb, savedImmutables: new Set() }

  // Initialize Renderer
  docWin.setTitle(getTitleText(docWindows[docWin.id]))
  await docWin.loadFile(path.join(__dirname, '/static/renderer.html'))
  await docWin.webContents.send('file-received', { filePath: filePath, fileData: fileData })

  // Prevent title being set from HTML
  docWin.on('page-title-updated', (evt) => {
    evt.preventDefault()
  })
}

/* ==== Helper Functions */

function getTitleText (windowInfo) {
  return `${path.basename(windowInfo.filePath)} - Gingko Writer`
}
