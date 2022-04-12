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
  let openFiles = {};

  ipcMain.on('clicked-new', async (event, title) => {
    isNew = true;
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    let d = new Date();
    let filename = `/home/adriano/Dropbox/Notes/testelectron${d}.md`;
    openFiles[filename] = await fs.open(filename, 'w');
    await win.loadFile(`${__dirname}/static/renderer.html`);
    await webContents.send('file-received', {filename : filename, fileData : null})
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
      let filename = dialogReturnValue.filePaths[0];
      isNew = false;
      openFiles[filename] = await fs.open(filename, 'r+');
      let fileData = await openFiles[filename].readFile({encoding: "utf8"});
      await win.loadFile(`${__dirname}/static/renderer.html`);
      await webContents.send('file-received', {filename : filename, fileData : fileData})
    }
  })

  ipcMain.on('set-dirty', (event, filename, isDirty) =>{
    console.log(filename, isDirty);
  })

  ipcMain.on('save-file', async (event, data) =>{
    await openFiles[data[0]].write(data[1], 0);
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