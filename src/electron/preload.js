const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  clickedNew: () => ipcRenderer.send('clicked-new', "somearg"),
  saveFile: (data) => ipcRenderer.send('save-file', data)
})