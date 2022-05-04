const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  localStoreSet: (key, value) => ipcRenderer.send('local-store-set', key, value),
  clickedNew: () => ipcRenderer.send('clicked-new', null),
  clickedOpen: () => ipcRenderer.send('clicked-open', null),
  clickedImport: () => ipcRenderer.send('clicked-import', null),
  clickedDocument: (docPath) => ipcRenderer.send('clicked-document', docPath),
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
