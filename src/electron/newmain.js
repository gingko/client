const { app, BrowserWindow } = require('electron');

console.log("Hello from new MAIN!")

const createWindow = () => {
  const win = new BrowserWindow({
    width: 800,
    height: 600
  })

  win.loadFile(`${__dirname}/static/home.html`);
}

app.whenReady().then(() => {
  createWindow()
})