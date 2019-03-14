export const tr =
  {

// === MENU ===

    file :
      { en : "&File"
      , es : "&Archivo"
      }
  , new :
      { en : "&New"
      , es : "&Nuevo"
      }
  , open :
      { en : "&Open..."
      , es : "&Abrir..."
      }
  , openRecent :
      { en : "Open &Recent"
      , es : "Abrir &Recientes"
      }
  , save :
      { en : "&Save"
      , es : "&Guardar"
      }
  , saved :
      { en : "Saved"
      , es : "Guardado"
      }
  , saveAs :
      { en : "Save &As..."
      , es : "Guardar Como..."
      }
  , importJSON :
      { en : "&Import JSON File..."
      , es : "&Importar Archivo JSON..."
      }
  , exportAsWord :
      { en : "Export as MS &Word"
      , es : "Exportar como MS &Word"
      }
  , exportAsText :
      { en : "Export as &Text"
      , es : "Exportar como &Texto"
      }
  , exportAsJSON :
      { en : "Export as &JSON..."
      , es : "Exportar como &JSON..."
      }
  , entireDocument :
      { en : "Entire Document..."
      , es : "Documento Entero..."
      }
  , currentSubtree :
      { en : "Current Card and Children..."
      , es : "Tarjeta Actual y Sus Hijos..."
      }
  , column :
      { en : (num) => `Column ${num}`
      , es : (num) => `Columna ${num}`
      }
  , repeatExport :
      { en : "Repeat Last E&xport"
      , es : "Repetir la última exportación"
      }
  , showList :
      { en : "Show Complete List..."
      , es : "Mostrar La Lista Entera..."
      }
  , close :
      { en : "&Close"
      , es :"&Cerrar"
      }
  , quit :
      { en : "&Quit Gingko..."
      , es : "&Salir de Gingko..."
      }
  , edit :
      { en : "&Edit"
      , es : "&Edición"
      }
  , undo :
      { en : "&Undo"
      , es : "&Deshacer"
      }
  , redo :
      { en : "&Redo"
      , es : "&Rehacer"
      }
  , cut :
      { en : "Cu&t"
      , es : "Cor&tar"
      }
  , copy :
      { en : "&Copy"
      , es : "&Copiar"
      }
  , paste :
      { en : "&Paste"
      , es : "&Pegar"
      }
  , selectAll :
      { en : "Select &All"
      , es : "&Seleccionar Todo"
      }
  , cutCards :
      { en : "Cu&t Cards"
      , es : "Cor&tar Tarjetas"
      }
  , copyCards :
      { en : "&Copy Cards"
      , es : "&Copiar Tarjetas"
      }
  , pasteCards :
      { en : "&Paste Cards"
      , es : "&Pegar Tarjetas"
      }
  , pasteCardsInto :
      { en : "Paste Cards as Children"
      , es : "Pegar Tarjetas como Hijos"
      }
  , view :
      { en : "&View"
      , es : "&Ver"
      }
  , selectFonts :
      { en : "Select &Fonts..."
      , es : "Seleccionar &Fuentes..."
      }
  , selectLanguage :
      { en : "Select &Language"
      , es : "Seleccionar &Idioma"
      }
  , zoomIn :
      { en : "&Zoom In"
      , es : "Acercar"
      }
  , zoomOut :
      { en : "Zoom &Out"
      , es : "Alejar"
      }
  , resetZoom :
      { en : "&Reset Zoom"
      , es : "Restablecer Zoom"
      }
  , toggleFullscreen :
      { en : "Toggle Full Screen"
      , es : "Alternar Pantalla Completa"
      }
  , help :
      { en : "&Help"
      , es : "Ayuda"
      }
  , faq :
      { en : "&FAQ..."
      , es : "&Preguntas Mas Frecuentes..."
      }
  , issuesList :
      { en : "Features &List && Known Bugs..."
      , es : "&Lista de Características y Errores Conocidos..."
      }
  , contactAdri :
      { en : "&Contact Adriano..."
      , es : "&Contactar Adriano..."
      }
  , buyLicense :
      { en : "&Buy a License..."
      , es : "Comprar una Licencia..."
      }
  , enterLicense :
      { en : "&Enter License..."
      , es : "Introducir Clave de Licencia..."
      }
  , openDevTools :
      { en : "Open &Debugging Tools"
      , es : "Abrir Herramientas de &Depuración"
      }

// === DIALOGS AND ERROR MESSAGES ===

  , cancel :
      { en : "Cancel"
      , es : "Cancelar"
      }

  , updatePopup :
      { en : (name, version) => `${name} will be updated to v${version} on exit`
      , es : (name, version) => `${name} se actualizará a v${version} cuando se cierre.`
      }

  , updatePopupBody :
      { en : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">Change list</a>:\n${notes}`
      , es : (notes) => `<a href="https://github.com/gingko/client/blob/master/CHANGELOG.md">Lista de cambios</a>:\n${notes}`
      }

  , areYouSureCancel :
      { en : "Are you sure you want to cancel your changes?"
      , es : "¿Seguro que quieres cancelar tus cambios?"
      }

  , unsavedChangesFound :
      { en : "Some Changes Weren't Saved"
      , es : "Algunos Cambios No Fueron Guardados"
      }

  , unsavedChangesMsg :
      { en : "Recover unsaved changes, or Discard them?"
      , es : "¿Recuperar cambios no guardados, o descartarlos?"
      }

  , discard :
      { en : "Discard Unsaved Changes"
      , es : "Descartar Cambios No Guardados"
      }

  , recover :
      { en : "Recover"
      , es : "Recupéralos"
      }

  , loadingError :
      { en : "Loading Error"
      , es : "Error al Abrir"
      }

  , loadingErrorMsg :
      { en : "Couldn't load file."
      , es : "No pude abrir el archivo."
      }

  , savingError :
      { en : "Save Error"
      , es : "Error al Guardar"
      }

  , savingErrorMsg :
      { en : "The file wasn't saved.\nPlease try again."
      , es : "El archivo no fue guardado.\nInténtalo de nuevo."
      }

  , exportError :
      { en : "Export Error"
      , es : "Error al Exportar"
      }

  , exportErrorMsg :
      { en : "Couldn't export.\nTry again."
      , es : "No se pudo exportar.\nInténtalo de nuevo."
      }
  };
