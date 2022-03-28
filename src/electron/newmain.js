const { app, BrowserWindow, ipcMain, dialog } = require('electron');
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

  let isNew;
  let filehandle;

  ipcMain.on('clicked-new', async (event, title) => {
    isNew = true;
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    let d = new Date();
    filehandle = await fs.open(`/home/adriano/Dropbox/Notes/testelectron${d}.md`, 'w')
    win.loadFile(`${__dirname}/static/renderer.html`);
  })

  ipcMain.on('clicked-open', async (event, title) => {
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)

    let dialogReturnValue = await dialog.showOpenDialog(win,
  { properties: ['openFile']
        , defaultPath: '/home/adriano/Dropbox/Notes/'
        , filters: [{name:'Markdown Document', extensions: ['md']}]
        });

    if (dialogReturnValue.filePaths.length != 0) {
      isNew = false;
      filehandle = await fs.open(dialogReturnValue.filePaths[0], 'r+');
      win.loadFile(`${__dirname}/static/renderer.html`);
    }
  })

  ipcMain.handle('get-loaded-file', async (event) =>{
    if (!isNew) {
      let fileData = await filehandle.readFile({encoding: "utf8"});
      return fileData;
    } else {
      console.log('no-loaded-file will be sent')
      return null;
    }
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