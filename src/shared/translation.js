export const tr =
  {

// === MENU ===

    file :
      { en : "&File"
      , zh : "&文件"
      , es : "&Archivo"
      , fr : ""
      }
  , new :
      { en : "&New"
      , zh : "&新的"
      , es : "&Nuevo"
      , fr : ""
      }
  , open :
      { en : "&Open..."
      , zh : "&打开……"
      , es : "&Abrir..."
      , fr : ""
      }
  , openRecent :
      { en : "Open &Recent"
      , zh : "打开&最新"
      , es : "Abrir &Recientes"
      , fr : ""
      }
  , menuSave :
      { en : "&Save"
      , zh : "&保存"
      , es : "&Guardar"
      , fr : ""
      }
  , saved :
      { en : "Saved"
      , zh : "已保存"
      , es : "Guardado"
      , fr : ""
      }
  , saveAs :
      { en : "Save &As..."
      , zh : "保存&为……"
      , es : "Guardar Como..."
      , fr : ""
      }
  , importJSON :
      { en : "&Import JSON File..."
      , zh : "&导入JSON 文件……"
      , es : "&Importar Archivo JSON..."
      , fr : ""
      }
  , exportAsWord :
      { en : "Export as MS &Word"
      , zh : "导出为MS&Word "
      , es : "Exportar como MS &Word"
      , fr : ""
      }
  , exportAsText :
      { en : "Export as &Text"
      , zh : "导出为&文本"
      , es : "Exportar como &Texto"
      , fr : ""
      }
  , exportAsJSON :
      { en : "Export as &JSON..."
      , zh : "导出为&JSON ……"
      , es : "Exportar como &JSON..."
      , fr : ""
      }
  , entireDocument :
      { en : "Entire Document..."
      , zh : "全部文件……"
      , es : "Documento Entero..."
      , fr : ""
      }
  , currentSubtree :
      { en : "Current Card and Children..."
      , zh : "当前卡和孩子……"
      , es : "Tarjeta Actual y Sus Hijos..."
      , fr : ""
      }
  , column :
      { en : (num) => `Column ${num}`
      , zh : (num) => `$列{num}`
      , es : (num) => `Columna ${num}`
      , fr : (num) => `${num}`
      }
  , repeatExport :
      { en : "Repeat Last E&xport"
      , zh : "重复上一次导出"
      , es : "Repetir la última exportación"
      , fr : ""
      }
  , showList :
      { en : "Show Complete List..."
      , zh : "显示完整列表……"
      , es : "Mostrar La Lista Entera..."
      , fr : ""
      }
  , close :
      { en : "&Close"
      , zh : "&关闭"
      , es :"&Cerrar"
      , fr : ""
      }
  , quit :
      { en : "&Quit Gingko..."
      , zh : "&退出Gingko ……"
      , es : "&Salir de Gingko..."
      , fr : ""
      }
  , edit :
      { en : "&Edit"
      , zh : "&编辑"
      , es : "&Edición"
      , fr : ""
      }
  , undo :
      { en : "&Undo"
      , zh : "&撤销"
      , es : "&Deshacer"
      , fr : ""
      }
  , redo :
      { en : "&Redo"
      , zh : "&重做"
      , es : "&Rehacer"
      , fr : ""
      }
  , cut :
      { en : "Cu&t"
      , zh : "断&开"
      , es : "Cor&tar"
      , fr : ""
      }
  , copy :
      { en : "&Copy"
      , zh : "&复制"
      , es : "&Copiar"
      , fr : ""
      }
  , paste :
      { en : "&Paste"
      , zh : "&粘贴"
      , es : "&Pegar"
      , fr : ""
      }
  , selectAll :
      { en : "Select &All"
      , zh : "选择&所有"
      , es : "&Seleccionar Todo"
      , fr : ""
      }
  , cutCards :
      { en : "Cu&t Cards"
      , zh : "切断&卡"
      , es : "Cor&tar Tarjetas"
      , fr : ""
      }
  , copyCards :
      { en : "&Copy Cards"
      , zh : "&复制卡"
      , es : "&Copiar Tarjetas"
      , fr : ""
      }
  , pasteCards :
      { en : "&Paste Cards"
      , zh : "&粘贴卡"
      , es : "&Pegar Tarjetas"
      , fr : ""
      }
  , pasteCardsInto :
      { en : "Paste Cards as Children"
      , zh : "将卡粘贴为孩子"
      , es : "Pegar Tarjetas como Hijos"
      , fr : ""
      }
  , view :
      { en : "&View"
      , zh : "&查看"
      , es : "&Ver"
      , fr : ""
      }
  , selectFonts :
      { en : "Select &Fonts..."
      , zh : "选择&字体"
      , es : "Seleccionar &Fuentes..."
      , fr : ""
      }
  , selectLanguage :
      { en : "Select &Language"
      , zh : "选择&语言"
      , es : "Seleccionar &Idioma"
      , fr : ""
      }
  , zoomIn :
      { en : "&Zoom In"
      , zh : " &放大"
      , es : "Acercar"
      , fr : ""
      }
  , zoomOut :
      { en : "Zoom &Out"
      , zh : "缩&小"
      , es : "Alejar"
      , fr : ""
      }
  , resetZoom :
      { en : "&Reset Zoom"
      , zh : "&重置缩放"
      , es : "Restablecer Zoom"
      , fr : ""
      }
  , toggleFullscreen :
      { en : "Toggle Full Screen"
      , zh : "切换全屏"
      , es : "Alternar Pantalla Completa"
      , fr : ""
      }
  , help :
      { en : "&Help"
      , zh : "&帮助"
      , es : "Ayuda"
      , fr : ""
      }
  , faq :
      { en : "&FAQ..."
      , zh : "&常见问题解答"
      , es : "&Preguntas Mas Frecuentes..."
      , fr : ""
      }
  , issuesList :
      { en : "Features &List && Known Bugs..."
      , zh : "功能&列表&&已知缺陷……"
      , es : "&Lista de Características y Errores Conocidos..."
      , fr : ""
      }
  , contactAdri :
      { en : "&Contact Adriano..."
      , zh : "&联系Adriano ……"
      , es : "&Contactar Adriano..."
      , fr : ""
      }
  , buyLicense :
      { en : "&Buy a License..."
      , zh : "&购买一个许可"
      , es : "Comprar una Licencia..."
      , fr : ""
      }
  , enterLicense :
      { en : "&Enter License..."
      , zh : "&输入许可……"
      , es : "Introducir Clave de Licencia..."
      , fr : ""
      }
  , backupFolder :
      { en : "Open Backups Folder..."
      , zh : "打开备份文件夹……"
      , es : "Abrir Archivos de \"Backup\"..."
      , fr : ""
      }
  , openDevTools :
      { en : "Open &Debugging Tools"
      , zh : "打开&调试工具"
      , es : "Abrir Herramientas de &Depuración"
      , fr : ""
      }
  , gingkoVersion :
      { en : (version) => `Current Gingko Version : ${version}`
      , zh : (version) => `$当前Gingko 版本: ${version}`
      , es : (version) => `Versión actual de Gingko : ${version}`
      , fr : (version) => `${version}`
      }

// === DIALOGS AND ERROR MESSAGES ===

  , cancel :
      { en : "Cancel"
      , zh : "取消"
      , es : "Cancelar"
      , fr : ""
      }

  , updatePopup :
      { en : (name, version) => `${name} will be updated to v${version} on exit`
      , zh : (name, version) => `${name} 将在退出时被更新至v ${version}`
      , es : (name, version) => `${name} se actualizará a v${version} cuando se cierre.`
      , fr : (name, version) => `${name} ${version}`
      }

  , updatePopupBody :
      { en : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">Change list</a>:\n${notes}`
      , zh : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">更改列表</a>:\n${notes}`
      , es : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">Lista de cambios</a>:\n${notes}`
      , fr : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md"></a>:\n${notes}`
      }

  , areYouSureCancel :
      { en : "Are you sure you want to cancel your changes?"
      , zh : "您确定要取消更改吗？"
      , es : "¿Seguro que quieres cancelar tus cambios?"
      , fr : ""
      }

  , save :
      { en : "Save"
      , zh : "保存"
      , es : "Guardar"
      , fr : ""
      }

  , saveChanges :
      { en : "Save changes"
      , zh : "保存更改"
      , es : "Guardar cambios"
      , fr : ""
      }

  , saveChangesMsg :
      { en : "Save changes before closing?"
      , zh : "在关闭前保存更改"
      , es : "¿Guardar cambios antes de cerrar?"
      , fr : ""
      }

  , unsavedChangesFound :
      { en : "Some Changes Weren't Saved"
      , zh : "一些未保存的更改"
      , es : "Algunos Cambios No Fueron Guardados"
      , fr : ""
      }

  , closeWithoutSaving :
      { en : "Close Without Saving"
      , zh : "不保存，直接关闭"
      , es : "Cerra Sin Guardar"
      , fr : ""
      }

  , unsavedChangesMsg :
      { en : "Recover unsaved changes, or Discard them?"
      , zh : "恢复未保存的更改，或放弃它们"
      , es : "¿Recuperar cambios no guardados, o descartarlos?"
      , fr : ""
      }

  , discard :
      { en : "Discard Unsaved Changes"
      , zh : "放弃未保存的更改"
      , es : "Descartar Cambios No Guardados"
      , fr : ""
      }

  , recover :
      { en : "Recover"
      , zh : "恢复"
      , es : "Recupéralos"
      , fr : ""
      }

  , loadingError :
      { en : "Loading Error"
      , zh : "载入时出错"
      , es : "Error al Abrir"
      , fr : ""
      }

  , loadingErrorMsg :
      { en : "Couldn't load file."
      , zh : "无法载入文件。"
      , es : "No pude abrir el archivo."
      , fr : ""
      }

  , saveError :
      { en : "Save Error"
      , zh : "保存时出错"
      , es : "Error al Guardar"
      , fr : ""
      }

  , saveErrorMsg :
      { en : "The file wasn't saved.\nPlease try again."
      , zh : "文件未保存。\ n请再试一次。"
      , es : "El archivo no fue guardado.\nInténtalo de nuevo."
      , fr : ""
      }

  , exportError :
      { en : "Export Error"
      , zh : "导出时出错"
      , es : "Error al Exportar"
      , fr : ""
      }

  , exportErrorMsg :
      { en : "Couldn't export.\nTry again."
      , zh : "无法导出。\ n请重试。"
      , es : "No se pudo exportar.\nInténtalo de nuevo."
      , fr : ""
      }
  };
