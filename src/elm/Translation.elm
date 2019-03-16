module Translation exposing (Language(..), TranslationId(..), langFromString, languageDecoder, timeDistInWords, tr)

import Json.Decode as Json exposing (..)
import Time
import Time.Distance as TimeDistance
import Time.Distance.I18n as I18n


type TranslationId
    = Cancel
    | HomeBlank
    | HomeImportJSON
    | HomeJSONFrom
    | RecentDocuments
    | LastOpened
    | OpenOtherDocuments
    | RemoveFromList
    | UnsavedChanges
    | AllChangesSaved
    | LastSaved
    | LastEdit
    | KeyboardHelp
    | RestoreThisVersion
    | EnterKey
    | EnterAction
    | EditCardTitle
    | ArrowsAction
    | AddChildAction
    | InsertChildTitle
    | AddBelowAction
    | InsertBelowTitle
    | AddAboveAction
    | InsertAboveTitle
    | ArrowKeys
    | MoveAction
    | Backspace
    | DeleteAction
    | DeleteCardTitle
    | FormattingGuide
    | ForBold
    | ForItalic
    | ToSaveChanges
    | SaveChangesTitle
    | EscKey
    | ToCancelChanges
    | PressToSearch
    | HeadingFont
    | ContentFont
    | EditingFont
    | WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int


type Language
    = En
    | Es


tr : Language -> TranslationId -> String
tr lang trans =
    let
        pluralize n str =
            if n == 1 then
                str

            else
                str ++ "s"

        translationSet =
            case trans of
                Cancel ->
                    { en = "Cancel"
                    , es = "Cancelar"
                    }

                HomeBlank ->
                    { en = "Blank"
                    , es = "Vacío"
                    }

                HomeImportJSON ->
                    { en = "Import JSON"
                    , es = "Importar JSON"
                    }

                HomeJSONFrom ->
                    { en = "From Desktop or Online"
                    , es = "Del App Escritorio o Web"
                    }

                RecentDocuments ->
                    { en = "Recent Documents"
                    , es = "Documentos Recientes"
                    }

                LastOpened ->
                    { en = "Last Opened"
                    , es = "Última Apertura"
                    }

                OpenOtherDocuments ->
                    { en = "Open Other Documents"
                    , es = "Abrir Otros Documentos"
                    }

                RemoveFromList ->
                    { en = "Remove From List"
                    , es = "Elminiar de la Lista"
                    }

                UnsavedChanges ->
                    { en = "Unsaved changes..."
                    , es = "Cambios no guardados..."
                    }

                AllChangesSaved ->
                    { en = "All changes saved"
                    , es = "Todos los cambios guardados"
                    }

                LastSaved ->
                    { en = "Last saved"
                    , es = "Guardado"
                    }

                LastEdit ->
                    { en = "Last edit"
                    , es = "Último cambio"
                    }

                KeyboardHelp ->
                    { en = "Keyboard Shortcuts Help"
                    , es = "Atajos de Teclado"
                    }

                RestoreThisVersion ->
                    { en = "Restore this Version"
                    , es = "Restaurar esta Versión"
                    }

                EnterKey ->
                    { en = "Enter"
                    , es = "Enter"
                    }

                EnterAction ->
                    { en = "to Edit"
                    , es = "para Editar"
                    }

                EditCardTitle ->
                    { en = "Edit Card (Enter)"
                    , es = "Editar Tarjeta (Enter)"
                    }

                ArrowsAction ->
                    { en = "to Navigate"
                    , es = "para Navegar"
                    }

                AddChildAction ->
                    { en = "to Add Child"
                    , es = "para Agregar un Hijo"
                    }

                InsertChildTitle ->
                    { en = "Insert Child (Ctrl+L)"
                    , es = "Agregar un Hijo (Ctrl+L)"
                    }

                AddBelowAction ->
                    { en = "to Add Below"
                    , es = "para Agregar Abajo"
                    }

                InsertBelowTitle ->
                    { en = "Insert Below (Ctrl+J)"
                    , es = "Agregar Abajo (Ctrl+J)"
                    }

                AddAboveAction ->
                    { en = "to Add Above"
                    , es = "para Agregar Arriba"
                    }

                InsertAboveTitle ->
                    { en = "Insert Above (Ctrl+K)"
                    , es = "Agregar Arriba (Ctrl+K)"
                    }

                ArrowKeys ->
                    { en = "(arrows)"
                    , es = "(flechas)"
                    }

                MoveAction ->
                    { en = "to Move"
                    , es = "para Mover"
                    }

                Backspace ->
                    { en = "Backspace"
                    , es = "Backspace"
                    }

                DeleteAction ->
                    { en = "to Delete"
                    , es = "para Eliminar"
                    }

                DeleteCardTitle ->
                    { en = "Delete Card (Ctrl+Backspace)"
                    , es = "Eliminar Tarjeta (Ctrl+Backspace)"
                    }

                FormattingGuide ->
                    { en = "Formatting Syntax Guide"
                    , es = "Guía de Sintaxis para Formato"
                    }

                ForBold ->
                    { en = "for Bold"
                    , es = "para Negrita"
                    }

                ForItalic ->
                    { en = "for Italic"
                    , es = "para Itálica"
                    }

                ToSaveChanges ->
                    { en = "to Save Changes"
                    , es = "para Guardar Cambios"
                    }

                SaveChangesTitle ->
                    { en = "Save Changes (Ctrl+Enter)"
                    , es = "Guardar Cambios (Ctrl+Enter)"
                    }

                EscKey ->
                    { en = "Esc"
                    , es = "Esc"
                    }

                ToCancelChanges ->
                    { en = "to Cancel Changes"
                    , es = "para Cancelar Cambios"
                    }

                PressToSearch ->
                    { en = "Press '/' to search"
                    , es = "Presiona '/' para buscar"
                    }

                HeadingFont ->
                    { en = "Heading Font"
                    , es = "Fuente de Titulos"
                    }

                ContentFont ->
                    { en = "Content Font"
                    , es = "Fuente de Contenido"
                    }

                EditingFont ->
                    { en = "Editing/Monospace Font"
                    , es = "Fuente de Edición"
                    }

                WordCountSession n ->
                    { en = "Session: " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Sesión: " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountTotal n ->
                    { en = "Total : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Total : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountCard n ->
                    { en = "Card : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Tarjeta : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountSubtree n ->
                    { en = "Subtree : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Subárbol : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountGroup n ->
                    { en = "Group : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Grupo : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountColumn n ->
                    { en = "Column : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Columna : " ++ String.fromInt n ++ pluralize n " palabra"
                    }
    in
    case lang of
        En ->
            .en translationSet

        Es ->
            .es translationSet


timeDistInWords : Language -> Time.Posix -> Time.Posix -> String
timeDistInWords lang t1 t2 =
    case lang of
        En ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Es ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.es t1 t2


languageDecoder : Decoder Language
languageDecoder =
    Json.map langFromString string


langFromString : String -> Language
langFromString str =
    case str of
        "en" ->
            En

        "es" ->
            Es

        _ ->
            En
