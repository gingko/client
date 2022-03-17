const { app, BrowserWindow, ipcMain } = require('electron');
const path = require('path')

console.log("Hello from new MAIN!")

const createWindow = () => {
  const win = new BrowserWindow({
    width: 800,
    height: 600,
    webPreferences: {
      preload: path.join(__dirname, 'preload.js')
    }
  })

  ipcMain.on('clicked-new', (event, title) => {
    const webContents = event.sender
    const win = BrowserWindow.fromWebContents(webContents)
    console.log("Clicked NEW (main)")
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