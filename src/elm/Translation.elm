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
    | SavedInternally
    | ChangesSaved
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
    | Zh
    | Es
    | Fr


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
                    , zh = "取消"
                    , es = "Cancelar"
                    , fr = ""
                    }

                HomeBlank ->
                    { en = "Blank"
                    , zh = ""
                    , es = "Vacío"
                    , fr = ""
                    }

                HomeImportJSON ->
                    { en = "Import JSON"
                    , zh = ""
                    , es = "Importar JSON"
                    , fr = ""
                    }

                HomeJSONFrom ->
                    { en = "From Desktop or Online"
                    , zh = ""
                    , es = "Del App Escritorio o Web"
                    , fr = ""
                    }

                RecentDocuments ->
                    { en = "Recent Documents"
                    , zh = ""
                    , es = "Documentos Recientes"
                    , fr = ""
                    }

                LastOpened ->
                    { en = "Last Opened"
                    , zh = ""
                    , es = "Última Apertura"
                    , fr = ""
                    }

                OpenOtherDocuments ->
                    { en = "Open Other Documents"
                    , zh = ""
                    , es = "Abrir Otros Documentos"
                    , fr = ""
                    }

                RemoveFromList ->
                    { en = "Remove From List"
                    , zh = ""
                    , es = "Elminiar de la Lista"
                    , fr = ""
                    }

                UnsavedChanges ->
                    { en = "Unsaved changes..."
                    , zh = ""
                    , es = "Cambios no guardados..."
                    , fr = ""
                    }

                SavedInternally ->
                    { en = "Saved Internally..."
                    , zh = ""
                    , es = "Cambios guardados internamente..."
                    , fr = ""
                    }

                ChangesSaved ->
                    { en = "Saved"
                    , zh = ""
                    , es = "Cambios guardados"
                    , fr = ""
                    }

                LastSaved ->
                    { en = "Last saved"
                    , zh = ""
                    , es = "Guardado"
                    , fr = ""
                    }

                LastEdit ->
                    { en = "Last edit"
                    , zh = ""
                    , es = "Último cambio"
                    , fr = ""
                    }

                KeyboardHelp ->
                    { en = "Keyboard Shortcuts Help"
                    , zh = ""
                    , es = "Atajos de Teclado"
                    , fr = ""
                    }

                RestoreThisVersion ->
                    { en = "Restore this Version"
                    , zh = ""
                    , es = "Restaurar esta Versión"
                    , fr = ""
                    }

                EnterKey ->
                    { en = "Enter"
                    , zh = ""
                    , es = "Enter"
                    , fr = ""
                    }

                EnterAction ->
                    { en = "to Edit"
                    , zh = ""
                    , es = "para Editar"
                    , fr = ""
                    }

                EditCardTitle ->
                    { en = "Edit Card (Enter)"
                    , zh = ""
                    , es = "Editar Tarjeta (Enter)"
                    , fr = ""
                    }

                ArrowsAction ->
                    { en = "to Navigate"
                    , zh = ""
                    , es = "para Navegar"
                    , fr = ""
                    }

                AddChildAction ->
                    { en = "to Add Child"
                    , zh = ""
                    , es = "para Agregar un Hijo"
                    , fr = ""
                    }

                InsertChildTitle ->
                    { en = "Insert Child (Ctrl+L)"
                    , zh = ""
                    , es = "Agregar un Hijo (Ctrl+L)"
                    , fr = ""
                    }

                AddBelowAction ->
                    { en = "to Add Below"
                    , zh = ""
                    , es = "para Agregar Abajo"
                    , fr = ""
                    }

                InsertBelowTitle ->
                    { en = "Insert Below (Ctrl+J)"
                    , zh = ""
                    , es = "Agregar Abajo (Ctrl+J)"
                    , fr = ""
                    }

                AddAboveAction ->
                    { en = "to Add Above"
                    , zh = ""
                    , es = "para Agregar Arriba"
                    , fr = ""
                    }

                InsertAboveTitle ->
                    { en = "Insert Above (Ctrl+K)"
                    , zh = ""
                    , es = "Agregar Arriba (Ctrl+K)"
                    , fr = ""
                    }

                ArrowKeys ->
                    { en = "(arrows)"
                    , zh = ""
                    , es = "(flechas)"
                    , fr = ""
                    }

                MoveAction ->
                    { en = "to Move"
                    , zh = ""
                    , es = "para Mover"
                    , fr = ""
                    }

                Backspace ->
                    { en = "Backspace"
                    , zh = ""
                    , es = "Backspace"
                    , fr = ""
                    }

                DeleteAction ->
                    { en = "to Delete"
                    , zh = ""
                    , es = "para Eliminar"
                    , fr = ""
                    }

                DeleteCardTitle ->
                    { en = "Delete Card (Ctrl+Backspace)"
                    , zh = ""
                    , es = "Eliminar Tarjeta (Ctrl+Backspace)"
                    , fr = ""
                    }

                FormattingGuide ->
                    { en = "Formatting Syntax Guide"
                    , zh = ""
                    , es = "Guía de Sintaxis para Formato"
                    , fr = ""
                    }

                ForBold ->
                    { en = "for Bold"
                    , zh = ""
                    , es = "para Negrita"
                    , fr = ""
                    }

                ForItalic ->
                    { en = "for Italic"
                    , zh = ""
                    , es = "para Itálica"
                    , fr = ""
                    }

                ToSaveChanges ->
                    { en = "to Save Changes"
                    , zh = ""
                    , es = "para Guardar Cambios"
                    , fr = ""
                    }

                SaveChangesTitle ->
                    { en = "Save Changes (Ctrl+Enter)"
                    , zh = ""
                    , es = "Guardar Cambios (Ctrl+Enter)"
                    , fr = ""
                    }

                EscKey ->
                    { en = "Esc"
                    , zh = ""
                    , es = "Esc"
                    , fr = ""
                    }

                ToCancelChanges ->
                    { en = "to Cancel Changes"
                    , zh = ""
                    , es = "para Cancelar Cambios"
                    , fr = ""
                    }

                PressToSearch ->
                    { en = "Press '/' to search"
                    , zh = ""
                    , es = "Presiona '/' para buscar"
                    , fr = ""
                    }

                HeadingFont ->
                    { en = "Heading Font"
                    , zh = ""
                    , es = "Fuente de Titulos"
                    , fr = ""
                    }

                ContentFont ->
                    { en = "Content Font"
                    , zh = ""
                    , es = "Fuente de Contenido"
                    , fr = ""
                    }

                EditingFont ->
                    { en = "Editing/Monospace Font"
                    , zh = ""
                    , es = "Fuente de Edición"
                    , fr = ""
                    }

                WordCountSession n ->
                    { en = "Session: " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Sesión: " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountTotal n ->
                    { en = "Total : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Total : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountCard n ->
                    { en = "Card : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Tarjeta : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountSubtree n ->
                    { en = "Subtree : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Subárbol : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountGroup n ->
                    { en = "Group : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Grupo : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }

                WordCountColumn n ->
                    { en = "Column : " ++ String.fromInt n ++ pluralize n " word"
                    , zh = ""
                    , es = "Columna : " ++ String.fromInt n ++ pluralize n " palabra"
                    , fr = ""
                    }
    in
    case lang of
        En ->
            .en translationSet

        Zh ->
            .zh translationSet

        Es ->
            .es translationSet

        Fr ->
            .fr translationSet


timeDistInWords : Language -> Time.Posix -> Time.Posix -> String
timeDistInWords lang t1 t2 =
    case lang of
        En ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Zh ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.en t1 t2

        Es ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.es t1 t2

        Fr ->
            TimeDistance.inWordsWithConfig { withAffix = True } I18n.fr t1 t2


languageDecoder : Decoder Language
languageDecoder =
    Json.map langFromString string


langFromString : String -> Language
langFromString str =
    case str of
        "en" ->
            En

        "zh" ->
            Zh

        "es" ->
            Es

        "fr" ->
            Fr

        _ ->
            En
