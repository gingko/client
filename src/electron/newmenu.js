function getMenuTemplate(handlers) {
  return [
    { label: 'File'
    , submenu:
      [ { label: 'New File'
        , accelerator: 'CommandOrControl+N'
        , click() {
            handlers.clickedNew()
          }
        }
      , { label: 'Open File'
        , accelerator: 'CommandOrControl+O'
        , click() {
            handlers.clickedOpen()
          }
        }
      ]
    }
  ];
}


module.exports =
  { getMenuTemplate
  };