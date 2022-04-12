const { app, BrowserWindow, ipcMain, dialog } = require('electron');
const path = require('path')
const crypto = require('crypto')
import * as fs from 'fs/promises';

let sha1Hash = crypto.createHash('sha1');

const createWindow = () => {
  const win = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  let isNew;
  let openWindows = {};

  ipcMain.on('clicked-new', async (event, title) => {
    isNew = true;
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)

    // Initialize New Document
    let d = new Date();
    let fileHash = sha1Hash.update(d.getTime()+"").digest('hex').slice(0,6);
    let filePath = path.join(app.getPath('temp'), `Untitled-${d.toISOString().slice(0,10)}-${fileHash}.gkw`);
    let filehandle = await fs.open(filePath, 'w');
    openWindows[win.id] = {filePath : filePath, filehandle : filehandle, dirty: true};

    // Prevent title being set from HTML
    win.on('page-title-updated', (evt) => {
      evt.preventDefault();
    });

    // Initialize Renderer
    await win.loadFile(`${__dirname}/static/renderer.html`);
    await webContents.send('file-received', {filePath : filePath, fileData : null})
    win.setTitle(getTitleText(openWindows[win.id]));

    // Prevent title being set from HTML
    win.on('page-title-updated', (evt) => {
      evt.preventDefault();
    });
  })

  ipcMain.on('clicked-open', async (event, title) => {
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)

    let dialogReturnValue = await dialog.showOpenDialog(win,
  { properties: ['openFile']
        , defaultPath: '/home/adriano/Dropbox/Notes/'
        , filters:
            [ {name:'Gingko Writer Document', extensions: ['gkw']}
            , {name:'Gingko Desktop Legacy', extensions: ['gko']}
            , {name:'Markdown Document', extensions: ['md']}
            ]
        });

    if (dialogReturnValue.filePaths.length != 0) {
      let filePath = dialogReturnValue.filePaths[0];
      isNew = false;
      let filehandle = await fs.open(filePath, 'r+');
      openWindows[win.id] = {filePath: filePath, filehandle: filehandle, dirty : false};

      // Load data
      let fileData = await openWindows[win.id].filehandle.readFile({encoding: "utf8"});

      // Initialize Renderer
      await win.loadFile(`${__dirname}/static/renderer.html`);
      await webContents.send('file-received', {filePath : filePath, fileData : fileData})
      win.setTitle(getTitleText(openWindows[win.id]));

      // Prevent title being set from HTML
      win.on('page-title-updated', (evt) => {
        evt.preventDefault();
      });
    }
  })

  ipcMain.on('set-dirty', (event, filePath, isDirty) =>{
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    if (openWindows[win.id].dirty !== isDirty) {
      console.log(isDirty);
      openWindows[win.id].dirty = isDirty;
      win.setTitle(getTitleText(openWindows[win.id]));
    }
  })

  ipcMain.on('save-file', async (event, data) =>{
    console.time('save-file')
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)

    await openWindows[win.id].filehandle.write(data[1], 0);
    console.timeEnd('save-file')
    openWindows[win.id].dirty = false;
    win.setTitle(getTitleText(openWindows[win.id]));
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


/* ==== Helper Functions */

function getTitleText (windowInfo) {
  let dirtyMarker = windowInfo.dirty ? "*" : "";
  return `${path.basename(windowInfo.filePath)}${dirtyMarker} - Gingko Writer`;
}