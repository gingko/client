const { shell , app , dialog } = require("electron");
const { tr } = require("../shared/translation.js");
const path = require("path");

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
    , editMenu(isDocument, isEditing, isMac, lang)
    , viewMenu(isDocument, lang, handlers)
    , helpMenu(handlers, isMac, lang)
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
    menuTemplate[0].submenu.push({type: "separator"}, {role: "quit", label: tr.quit[lang]} );
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
    , { label: (isNew ? tr.menuSave[lang] : (isChanged ? tr.menuSave[lang] : tr.saved[lang]))
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
      , { label: tr.exportAsWord[lang]
        , submenu : exportMenu("docx", columnNumber, lang)
        }
      , { label: tr.exportAsText[lang]
        , submenu : exportMenu("txt", columnNumber, lang)
        }
      , { label: tr.exportAsJSON[lang]
        , click : function (item, focusedWindow) {
            focusedWindow.webContents.send("menu-export-json");
          }
        }
      , { label: tr.repeatExport[lang]
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




function exportMenu (format, cols, lang) {
  let _expMenu =
    [ { label : tr.entireDocument[lang]
      , click : function (item, focusedWindow) {
        focusedWindow.webContents.send(`menu-export-${format}`);
      }
    }
      , { label : tr.currentSubtree[lang]
        , click : function (item, focusedWindow) {
          focusedWindow.webContents.send(`menu-export-${format}-current`);
        }
      }
      , { type: "separator" }
    ];

  let expMenuItem = function (num) {
    return { label : tr.column[lang](num)
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





function editMenu (isDocument, isEditing, isMac, lang) {
  let _editMenu;

  if ( (isDocument && isEditing) || !isDocument ) {
    _editMenu =
      { label: tr.edit[lang]
      , submenu:
          [ { role: "undo" , label : tr.undo[lang] }
          , { role: "redo" , label : tr.redo[lang] }
          , { type: "separator" }
          , { role: "cut" , label : tr.cut[lang] }
          , { role: "copy", label : tr.copy[lang] }
          , { role: "paste" , label : tr.paste[lang] }
          , { role: "selectAll", label : tr.selectAll[lang] }
          ]
      };
  } else {
    _editMenu =
      { label: tr.edit[lang]
      , submenu:
          [ { label: tr.undo[lang]
            , accelerator : "CommandOrControl+Z"
            , click : function (item, focusedWindow) {
                focusedWindow.webContents.send("menu-undo");
              }
            }
          , { label: tr.redo[lang]
            , accelerator : "CommandOrControl+Shift+Z"
            , click : function (item, focusedWindow) {
                focusedWindow.webContents.send("menu-redo");
              }
            }
          , { type: "separator" }
          , { label: tr.cutCards[lang]
            , accelerator : "CommandOrControl+X"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-cut");
                }
              }
            }
          , { label: tr.copyCards[lang]
            , accelerator : "CommandOrControl+C"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-copy");
                }
              }
            }
          , { label: tr.pasteCards[lang]
            , accelerator : "CommandOrControl+V"
            , click : function (item, focusedWindow) {
                if (!isMac) {
                  focusedWindow.webContents.send("menu-paste");
                }
              }
            }
          , { label: tr.pasteCardsInto[lang]
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
    { label: tr.view[lang]
    , submenu:
        [ { label: tr.selectFonts[lang]
          , enabled: isDocument
          , click : handlers.fonts
          }
        , { label : tr.selectLanguage[lang]
          , submenu :
          [ { label : "English", type: "radio", checked: lang == "en", click : (item, focusedWindow) => { handlers.language("en", focusedWindow);} }
          , { label : "Español", type: "radio", checked: lang == "es", click : (item, focusedWindow) => { handlers.language("es", focusedWindow);} }
          ]
          }
        , { type : "separator" }
        , { label: tr.zoomIn[lang]
          , enabled: isDocument
          , accelerator: "CommandOrControl+="
          , click : function (item, focusedWindow) {
              let contents = focusedWindow.webContents;
              contents.getZoomLevel(level => {
                contents.setZoomLevel(level + 1);
              });
            }
          }
        , { label: tr.zoomOut[lang]
          , enabled: isDocument
          , accelerator: "CommandOrControl+-"
          , click : function (item, focusedWindow) {
              let contents = focusedWindow.webContents;
              contents.getZoomLevel(level => {
                contents.setZoomLevel(level - 1);
              });
            }
          }
        , { label: tr.resetZoom[lang]
          , enabled: isDocument
          , accelerator: "CommandOrControl+0"
          , click : function (item, focusedWindow) {
              focusedWindow.webContents.setZoomLevel(0);
            }
          }
        , { type: "separator" }
        , { role: "togglefullscreen", label : tr.toggleFullscreen[lang] }
        ]
    };

  return _viewMenu;
}



function helpMenu(handlers, isMac, lang) {
  let _helpMenu;

  _helpMenu =
    { label: tr.help[lang]
    , submenu:
        [ { label: tr.faq[lang]
          , click : () => shell.openExternal("https://gingko.io/faq.html")
          }
        , { label: tr.issuesList[lang]
          , click : () => shell.openExternal("https://github.com/gingko/client/issues")
          }
        , { label: tr.contactAdri[lang]
          , click : (item, focusedWindow) => focusedWindow.webContents.send("menu-contact-support")
          }
        , { type: "separator" }
        , { label: tr.buyLicense[lang]
          , click : () => shell.openExternal("https://gingkoapp.com/desktop-upgrade")
          }
        , { label: tr.enterLicense[lang]
          , click : handlers.enterLicense
          }
        , { type: "separator" }
        , { label : tr.backupFolder[lang]
          , click : () => shell.openItem(path.join(app.getPath("userData"), "backups"))
          }
        , { label: tr.openDevTools[lang]
          , accelerator: isMac ? "Alt+Command+I" : "Ctrl+Shift+I"
          , click : function (item, focusedWindow) {
              if (focusedWindow) focusedWindow.webContents.toggleDevTools();
            }
          }
        , { type: "separator" }
        , { label : tr.gingkoVersion[lang](app.getVersion()) , enabled : false }
        ]
    };

  return _helpMenu;
}
