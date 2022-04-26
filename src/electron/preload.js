const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  clickedNew: () => ipcRenderer.send('clicked-new', 'somearg'),
  clickedOpen: () => ipcRenderer.send('clicked-open', 'somearg'),
  clickedExport: (callback) => ipcRenderer.on('clicked-export', callback),
  fileReceived: (callback) => ipcRenderer.on('file-received', callback),
  fileSaved: (callback) => ipcRenderer.on('file-saved', callback),
  exportFile: (data) => ipcRenderer.send('export-file', data),
  saveFile: (data) => ipcRenderer.send('save-file', data),
  commitData: (commitData) => ipcRenderer.send('commit-data', commitData),
  commitDataResult: (callback) => ipcRenderer.on('commit-data-result', callback),
  maybeCloseWindow: () => ipcRenderer.send('maybe-close-window'),
  closeWindow: () => ipcRenderer.send('close-window')
})
