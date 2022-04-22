import * as fs from 'fs/promises'
import { app, BrowserWindow, ipcMain, dialog, Menu } from 'electron'
import { getHomeMenuTemplate, getDocMenuTemplate } from './newmenu'
const path = require('path')
const crypto = require('crypto')

const sha1Hash = crypto.createHash('sha1')
const docWindows = {}

const createHomeWindow = () => {
  const handlers = {
    clickedNew: (item, focusedWindow) => clickedNew(focusedWindow),
    clickedOpen: (item, focusedWindow) => clickedOpen(focusedWindow)
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
  win.hide()
  await createDocWindow(null)
  win.destroy()
}

async function clickedOpen (win) {
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
    win.hide()
    await createDocWindow(dialogReturnValue.filePaths[0])
    win.destroy()
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
  clickedOpen(homeWindow)
})

ipcMain.on('save-untitled', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const { filePath, canceled } = await dialog.showSaveDialog(win, { defaultPath: app.getPath('documents') })
  if (!canceled && filePath) {
    docWindows[win.id].filePath = filePath
    await fs.writeFile(filePath, data[1])
    await webContents.send('file-saved', filePath)
    win.setTitle(getTitleText(docWindows[win.id]))
  }
})

ipcMain.on('save-file', async (event, data) => {
  const webContents = event.sender
  const win = BrowserWindow.fromWebContents(webContents)

  const filePath = docWindows[win.id].filePath
  await fs.writeFile(filePath, data[1])
  await webContents.send('file-saved', filePath)
  win.setTitle(getTitleText(docWindows[win.id]))
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

async function createDocWindow (filePath) {
  const handlers = { clickedNew: () => true, clickedOpen: () => true }
  const template = Menu.buildFromTemplate(getDocMenuTemplate(handlers))
  Menu.setApplicationMenu(template)

  const docWin = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let fileData = null
  if (filePath == null) {
    // Initialize New Document
    const d = new Date()
    const fileHash = sha1Hash.update(d.getTime() + '').digest('hex').slice(0, 6)
    filePath = path.join(app.getPath('temp'), `Untitled-${d.toISOString().slice(0, 10)}-${fileHash}.gkw`)
  } else {
    // Load Document
    fileData = await fs.readFile(filePath, { encoding: 'utf8' })
  }
  docWindows[docWin.id] = { filePath: filePath }

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
