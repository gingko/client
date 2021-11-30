module Import.Text exposing (Model, Msg, init, setFileList, toTree, update, view)

import Doc.TreeStructure as TreeStructure
import File exposing (File)
import File.Select
import Html exposing (Html, button, div, input, label, li, text, ul)
import Html.Attributes exposing (checked, class, disabled, for, id, style, type_, value)
import Html.Events exposing (on, onClick)
import Html.Extra exposing (viewIf)
import List.Extra as ListExtra
import Random
import RandomId
import Regex
import SharedUI exposing (modalWrapper)
import Task
import Types exposing (Children(..), Tree)



-- MODEL


type alias Model =
    { files : List File
    , importSettings : ImportSettings
    }


type ImportSettings
    = CardPerFile Bool
    | Split Separator


type Separator
    = Paragraph
    | Other String


init : Model
init =
    { files = [], importSettings = CardPerFile True }



-- UPDATE


type Msg
    = FilesRequested
    | GotFiles File (List File)
    | SetCardPerFile Bool
    | SetSplit Separator
    | RequestImport


update : Msg -> Model -> { model : Model, cmd : Cmd Msg, sendTestHack : Bool, importRequested : Maybe (List File) }
update msg model =
    case msg of
        FilesRequested ->
            { model = model
            , cmd = File.Select.files [ ".md", ".markdown", ".mdown", "text/markdown", "text/x-markdown", "text/plain" ] GotFiles
            , sendTestHack = True
            , importRequested = Nothing
            }

        GotFiles head rest ->
            { model = { model | files = head :: rest }
            , cmd = Cmd.none
            , sendTestHack = False
            , importRequested = Nothing
            }

        SetCardPerFile fileTitle ->
            { model = { model | importSettings = CardPerFile fileTitle }, cmd = Cmd.none, sendTestHack = False, importRequested = Nothing }

        SetSplit sep ->
            { model = { model | importSettings = Split sep }, cmd = Cmd.none, sendTestHack = False, importRequested = Nothing }

        RequestImport ->
            { model = model, cmd = Cmd.none, sendTestHack = False, importRequested = Just model.files }


setFileList : List File -> Model -> Model
setFileList files model =
    { model | files = files }



-- VIEW


view : { closeMsg : msg, tagger : Msg -> msg } -> Model -> List (Html msg)
view msgs model =
    let
        ( isCardPerFile, isFilenameTitle, isParagraph ) =
            case model.importSettings of
                CardPerFile fileTitle ->
                    ( True, fileTitle, False )

                Split sep ->
                    ( False, True, sep == Paragraph )
    in
    [ button [ id "import-text-file-input", onClick (msgs.tagger FilesRequested) ] [ text "Browse Files" ]
    , ul []
        [ li []
            [ input [ type_ "radio", id "card-per-file-radio", checked isCardPerFile, onClick (msgs.tagger (SetCardPerFile True)) ] []
            , label [ for "card-per-file-radio" ] [ text "One card per file" ]
            , viewIf isCardPerFile <| div [] [ input [ id "filename-as-card-title", type_ "checkbox", checked isFilenameTitle ] [], label [ for "filename-as-card-title" ] [ text "Use Filename as Card Title" ] ]
            ]
        , li []
            [ input [ type_ "radio", id "split-import", checked (not isCardPerFile), onClick (msgs.tagger (SetSplit Paragraph)) ] []
            , label [ for "split-import" ] [ text "Split Into Multiple Cards" ]
            , viewIf (not isCardPerFile) <|
                div [ style "display" "flex" ]
                    [ input [ id "split-by-paragraph", type_ "radio", checked isParagraph, onClick (msgs.tagger (SetSplit Paragraph)) ] []
                    , label [ for "split-by-paragraph" ] [ text "Split at Paragraph Breaks & New Lines" ]
                    , input [ id "split-by-separator", type_ "radio", checked (not isParagraph), onClick (msgs.tagger (SetSplit (Other "---"))) ] []
                    , label [ for "split-by-separator" ] [ text "Split by Separator : ", input [ id "separator-input", value "---" ] [] ]
                    ]
            ]
        ]
    , button [ id "import-text-perform", disabled (List.isEmpty model.files), onClick (msgs.tagger RequestImport) ] [ text "Import" ]
    ]
        |> modalWrapper msgs.closeMsg (Just "import-text-modal") Nothing "Import Text Files"


toTree : Random.Seed -> List String -> List String -> ( Tree, Random.Seed )
toTree seed metadata markdownStrings =
    let
        ( salt, newSeed ) =
            Random.step RandomId.stringGenerator seed

        filenameAndContent =
            ListExtra.zip metadata markdownStrings

        removeExtensionRegex_ =
            Regex.fromString "\\..*$"

        titleContentToString title content =
            case removeExtensionRegex_ of
                Just regex ->
                    "# " ++ (title |> Regex.replace regex (\_ -> "")) ++ "\n" ++ content

                Nothing ->
                    "# " ++ title ++ "\n" ++ content

        mapFn idx ( title, content ) =
            Tree (String.fromInt (idx + 1))
                (titleContentToString title content)
                (Children [])

        newTree =
            filenameAndContent
                |> List.indexedMap mapFn
                |> Children
                |> Tree "0" ""
                |> TreeStructure.renameNodes salt
    in
    ( newTree, newSeed )
