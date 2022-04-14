const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  clickedNew: () => ipcRenderer.send('clicked-new', 'somearg'),
  clickedOpen: () => ipcRenderer.send('clicked-open', 'somearg'),
  fileReceived: (callback) => ipcRenderer.on('file-received', callback),
  fileSaved: (callback) => ipcRenderer.on('file-saved', callback),
  saveUntitled: (data) => ipcRenderer.send('save-untitled', data),
  saveFile: (data) => ipcRenderer.send('save-file', data),
  closeWindow: () => ipcRenderer.send('close-window')
})
