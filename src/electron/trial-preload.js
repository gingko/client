const { contextBridge } = require('electron')

contextBridge.exposeInMainWorld('electronAPI', {
  getArgs: () => {
    return window.process.argv
      .filter((arg) => arg.startsWith('trialarg='))
      .map((arg) => Number(arg.slice(9)))

  }
})
