const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  clickedNew: () => ipcRenderer.send('clicked-new', "somearg"),
  clickedOpen: () => ipcRenderer.send('clicked-open', "somearg"),
  fileReceived: (callback) => ipcRenderer.on('file-received', callback),
  setDirty: (filePath, isDirty) => ipcRenderer.send('set-dirty', filePath, isDirty),
  saveFile: (data) => ipcRenderer.send('save-file', data)
})