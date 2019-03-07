const { shell } = require("electron");
const { tr } = require("../shared/translation.js");


function getTemplate (menuState, handlers, lang, isMac) {
  let isDocument = !!menuState;
  let isNew = menuState && menuState.isNew;
  let isEditing = menuState && menuState.editMode;
  let columnNumber = menuState && menuState.columnNumber;
  let changed = menuState && menuState.changed;
  let hasLastExport = menuState && menuState.hasLastExport;
  let recentDocumentList = ( menuState && menuState.recentDocumentList ) || [];
  lang = lang || "en";

  let menuTemplate =
    [ fileMenu(isDocument, isNew, changed, columnNumber, hasLastExport, recentDocumentList, lang, handlers)
    , editMenu(isDocument, isEditing, isMac)
    , viewMenu(isDocument, lang, handlers)
    , helpMenu(handlers, isMac)
    ];


  if (isMac) {
    menuTemplate.unshift(
      { label : "Gingko"
      , submenu :
          [ {role: "about"}
          , {type: "separator"}
          , {role: "services", submenu: []}
          , {type: "separator"}
          , {role: "hide"}
          , {role: "hideothers"}
          , {role: "unhide"}
          , {type: "separator"}
          , { label: tr.quit[lang]
            , accelerator: "Command+Q"
            , click : handlers.quit
            }
          ]
      });

    menuTemplate.splice(4, 0, { role: "windowMenu"});
  } else {
    let closeMenuItem = { label : tr.close[lang], accelerator: "Ctrl+W", click : function (item, focusedWindow) { focusedWindow.webContents.send("menu-close-document"); }};
    menuTemplate[0].submenu.splice(3, 0, closeMenuItem);
    menuTemplate[0].submenu.push({type: "separator"}, {role: "quit"} );
  }

  return menuTemplate;
}


module.exports = getTemplate;




/* PRIVATE FUNCTIONS */


function fileMenu (isDocument, isNew, isChanged, columnNumber, hasLastExport, recentDocumentList, lang, handlers) {
  let _fileMenu;

  let recentDocsMenu =
    recentDocumentList.map((rdoc, idx) => {
      return { label : (idx < 9 ? `&${idx+1}  ` : "   ") + rdoc.name, click : () => { handlers.openRecent(rdoc); }};
    });

  recentDocsMenu = recentDocsMenu.slice(0,20);
  recentDocsMenu.push({ type: "separator" });
  recentDocsMenu.push({ label: tr.showList[lang], click: handlers.openHome });


  let _subMenu =
    [ { label : tr.new[lang]
      , accelerator : "CmdOrCtrl+N"
      , click : handlers.new
      }
    , { label: tr.open[lang]
      , accelerator: "CmdOrCtrl+O"
      , click: handlers.open
      }
    , { label: tr.openRecent[lang]
      , submenu : recentDocsMenu
      }
    , { type: "separator" }
    , { label: (isNew ? tr.save[lang] : (isChanged ? tr.save[lang] : tr.saved[lang]))
      , enabled: isNew || isChanged
      , accelerator: "CmdOrCtrl+S"
      , click : isChanged ? handlers.save : handlers.saveAs
      }
    , { label: tr.saveAs[lang]
      , accelerator: "CmdOrCtrl+Shift+S"
      , click : handlers.saveAs
      }
    , { type: "separator" }
    , { label: tr.importJSON[lang]
      , click : handlers.import
      }
    ];


  if (isDocument) {
    _subMenu = _subMenu.concat(
      [ { type: "separator" }
      , { label: "Export as MS &Word"
        , submenu : exportMenu("docx", columnNumber)
        }
      , { label: "Export as &Text"
        , submenu : exportMenu("txt", columnNumber)
        }
      , { label: "Export as &JSON..."
        , click : function (item, focusedWindow) {
            focusedWindow.webContents.send("menu-export-json");
          }
        }
      , { label: "Repeat Last E&xport"
        , enabled: hasLastExport
        , accelerator: "CommandOrControl+r"
        , click : function (item, focusedWindow) {
            focusedWindow.webContents.send("menu-export-repeat");
          }
        }
      ]
    );
  }

  _fileMenu =
    { label : tr.file[lang]
    , submenu : _subMenu
    };

  return _fileMenu;
}




function exportMenu (format, cols) {
  let _expMenu =
    [ { label : "Entire Document..."
      , click : function (item, focusedWindow) {
        focusedWindow.webContents.send(`menu-export-${format}`);
      }
    }
      , { label : "Current Card and Children..."
        , click : function (item, focusedWindow) {
          focusedWindow.webContents.send(`menu-export-${format}-current`);
        }
      }
      , { type: "separator" }
    ];

  let expMenuItem = function (num) {
    return { label : `Column ${num}...`
      , click : function (item, focusedWindow) {
        focusedWindow.webContents.send(`menu-export-${format}-column`, num);
      }
    };
  };

  for (var i = 1; i <= cols;i++) {
    _expMenu.push(expMenuItem(i));
  }

  return _expMenu;
}





function editMenu (isDocument, isEditing, isMac) {
  let _editMenu;

  if ( (isDocument && isEditing) || !isDocument ) {
    _editMenu =
      { label: "&Edit"
      , submenu:
          [ { role: "undo" }
          , { role: "redo" }
          , { type: "separator" }
          , { role: "cut" }
          , { role: "copy" }
          , { role: "paste" }
          , { role: "selectAll"}
          ]
      };
  } else {
    _editMenu =
      { label: "&Edit"
      , submenu:
          [ { label: "&Undo"
            , accelerator : "CommandOrControl+Z"
            , click : function (item, focusedWindow) {
                focusedWindow.webContents.send("menu-undo");
              }
            }
          , { label: "&Redo"
            , accelerator : "CommandOrControl+Shift+Z"
            , click : function (item, focusedWindow) {
                focusedWindow.webContents.send("menu-redo");
              }
            }
          , { type: "separator" }
          , { label: "Cu&t Cards"
            , accelerator : "CommandOrControl+X"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-cut");
                }
              }
            }
          , { label: "&Copy Cards"
            , accelerator : "CommandOrControl+C"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-copy");
                }
              }
            }
          , { label: "&Paste Cards"
            , accelerator : "CommandOrControl+V"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-paste");
                }
              }
            }
          , { label: "Paste Cards as Children"
            , accelerator : "CommandOrControl+Shift+V"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-paste-into");
                }
              }
            }
          ]
      };
  }

  return _editMenu;
}




function viewMenu(isDocument, lang, handlers) {
  let _viewMenu =
    { label: "&View"
    , submenu:
        [ { label: "Select &Fonts..."
          , enabled: isDocument
          , click : handlers.fonts
          }
        , { label : "Select &Language"
          , submenu :
          [ { label : "English", type: "radio", checked: lang == "en", click : (item, focusedWindow) => { handlers.language("en", focusedWindow);} }
          , { label : "Español", type: "radio", checked: lang == "es", click : (item, focusedWindow) => { handlers.language("es", focusedWindow);} }
          ]
          }
        , { type : "separator" }
        , { label: "&Zoom In"
          , enabled: isDocument
          , accelerator: "CommandOrControl+="
          , click : function (item, focusedWindow) {
              let contents = focusedWindow.webContents;
              contents.getZoomLevel(level => {
                contents.setZoomLevel(level + 1);
              });
            }
          }
        , { label: "Zoom &Out"
          , enabled: isDocument
          , accelerator: "CommandOrControl+-"
          , click : function (item, focusedWindow) {
              let contents = focusedWindow.webContents;
              contents.getZoomLevel(level => {
                contents.setZoomLevel(level - 1);
              });
            }
          }
        , { label: "&Reset Zoom"
          , enabled: isDocument
          , accelerator: "CommandOrControl+0"
          , click : function (item, focusedWindow) {
              focusedWindow.webContents.setZoomLevel(0);
            }
          }
        , { type: "separator" }
        , { role: "togglefullscreen" }
        ]
    };

  return _viewMenu;
}



function helpMenu(handlers, isMac) {
  let _helpMenu;

  _helpMenu =
    { label: "&Help"
    , submenu:
        [ { label: "&FAQ..."
          , click : () => shell.openExternal("https://gingko.io/faq.html")
          }
        , { label: "Features &List && Known Bugs..."
          , click : () => shell.openExternal("https://github.com/gingko/client/issues")
          }
        , { label: "&Contact Adriano..."
          , click : (item, focusedWindow) => focusedWindow.webContents.send("menu-contact-support")
          }
        , { type: "separator" }
        , { label: "&Buy a License..."
          , click : () => shell.openExternal("https://gingkoapp.com/desktop-upgrade")
          }
        , { label: "&Enter License..."
          , click : handlers.enterLicense
          }
        , { type: "separator" }
        , { label: "Open &Debugging Tools"
          , accelerator: isMac ? "Alt+Command+I" : "Ctrl+Shift+I"
          , click : function (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.toggleDevTools();
            }
          }
        ]
    };

  return _helpMenu;
}
