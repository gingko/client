const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  clickedNew: () => ipcRenderer.send('clicked-new', "somearg"),
  clickedOpen: () => ipcRenderer.send('clicked-open', "somearg"),
  getLoadedFile: () => ipcRenderer.invoke('get-loaded-file'),
  saveFile: (data) => ipcRenderer.send('save-file', data)
})