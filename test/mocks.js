module.exports = function (dialog, Menu) {
  if(!process.env.RUNNING_IN_SPECTRON) { return; }

  if (typeof process.env.DIALOG_CHOICE === "string") {
    dialog.showMessageBox = () => {
      return { response: Number(process.env.DIALOG_CHOICE) };
    };
  }

  if (typeof process.env.DIALOG_SAVE_PATH === "string") {
    dialog.showSaveDialog = () => {
      return { filePath: process.env.DIALOG_SAVE_PATH };
    };
  }

  if (typeof process.env.DIALOG_OPEN_PATH === "string") {
    dialog.showOpenDialog = async () => {
      return { filePaths: [process.env.DIALOG_OPEN_PATH] };
    };
  }

  if (typeof process.env.MENU_ITEM_ID === "string") {
    let originalFn = Menu.buildFromTemplate;
    let newFn = (template) => {
      template.map((mi) => {
        if (mi.id == process.env.MENU_ITEM_ID) {
          mi.accelerator = process.env.MENU_ITEM_ACCELERATOR;
        }
      });

      return originalFn(template);
    };
    Menu.buildFromTemplate = newFn;
  }
};
