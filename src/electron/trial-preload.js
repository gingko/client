const { contextBridge, ipcRenderer } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  getArgs: () => {
    return window.process.argv
      .filter((arg) => arg.startsWith('trialarg='))
      .map((arg) => {
        const argVal = arg.slice(9)
        const asNum = Number(argVal)
        if (Number.isNaN(asNum)) {
          return argVal
        } else {
          return asNum
        }
      })
  },
  closeWindow: () => { ipcRenderer.send('close-window') }
})
