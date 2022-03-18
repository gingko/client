const { app, BrowserWindow, ipcMain, protocol } = require('electron');
const path = require('path')
import * as fs from 'fs/promises';

const createWindow = () => {
  const win = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let filehandle;

  ipcMain.on('clicked-new', async (event, title) => {
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    filehandle = await fs.open('/home/adriano/Documents/testelectron.md', 'w')
    win.loadFile(`${__dirname}/static/renderer.html`);
  })

  ipcMain.on('save-file', async (event, data) =>{
    await filehandle.write(data, 0);
  })

  win.loadFile(`${__dirname}/static/home.html`);
}

app.whenReady().then(() => {
  createWindow()

  app.on('activate', () => {
    // On macOS re-create a window when the dock icon is clicked
    // and there are no other windows open.
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

// Quit when all windows are closed, except on macOS.
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})