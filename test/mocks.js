module.exports = function(dialog, mockChoice, mockSavePath, mockOpenPathArray) {
  dialog.showMessageBox = (options) => {
    return mockChoice
  }

  dialog.showSaveDialog = (options) => {
    return mockSavePath
  }

  dialog.showOpenDialog = (options) => {
    return mockOpenPathArray
  }
}
