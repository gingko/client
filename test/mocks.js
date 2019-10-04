module.exports = function(dialog, mockChoice, mockSavePath, mockOpenPathArray) {
  dialog.showMessageBox = (options) => {
    return { response: Number(mockChoice) };
  }

  dialog.showSaveDialog = (options) => {
    return { filePath: mockSavePath };
  }

  dialog.showOpenDialog = async (options) => {
    return { filePaths: mockOpenPathArray };
  }
}
