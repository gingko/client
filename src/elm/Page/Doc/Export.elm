module Page.Doc.Export exposing (ExportFormat(..), ExportSelection(..), command, exportView, exportViewError, toExtension, toString)

import Ant.Icons.Svg as AntIcons
import Api
import Bytes exposing (Bytes)
import Coders exposing (treeToJSON, treeToMarkdownString, treeToOPML)
import Doc.TreeUtils exposing (getColumnById, getLeaves)
import File.Download as Download
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (attribute, class, id)
import Html.Events exposing (onClick, onMouseEnter, onMouseLeave)
import Http
import Json.Encode as Enc
import Markdown
import Translation exposing (TranslationId(..))
import Types exposing (Children(..), TooltipPosition(..), Tree)


type ExportSelection
    = ExportEverything
    | ExportSubtree
    | ExportLeaves
    | ExportCurrentColumn


type ExportFormat
    = PlainText
    | DOCX
    | OPML
    | JSON


command : (String -> Result Http.Error Bytes -> msg) -> String -> String -> ( ExportSelection, ExportFormat ) -> Tree -> Tree -> Cmd msg
command exportedMsg docId docName (( _, exportFormat ) as exportSettings) activeTree fullTree =
    let
        exportedString =
            toString docName exportSettings activeTree fullTree
    in
    case exportFormat of
        JSON ->
            Download.string (docName ++ ".json") "application/json" exportedString

        OPML ->
            Download.string (docName ++ ".opml") "application/xml, text/xml, text/x-opml" exportedString

        PlainText ->
            Download.string (docName ++ ".txt") "text/plain" exportedString

        DOCX ->
            Api.exportDocx
                (exportedMsg docName)
                { docId = docId, markdown = exportedString }


toString : String -> ( ExportSelection, ExportFormat ) -> Tree -> Tree -> String
toString docName ( exportSelection, exportFormat ) activeTree fullTree =
    let
        stringFn withRoot tree =
            case exportFormat of
                JSON ->
                    treeToJSON withRoot tree
                        |> Enc.encode 2

                OPML ->
                    treeToOPML docName tree

                _ ->
                    treeToMarkdownString withRoot tree

        currentColumnCards =
            getColumnById activeTree.id fullTree
                |> Maybe.withDefault []
                |> List.concat
                |> List.map (\c -> { c | children = Children [] })
    in
    case exportSelection of
        ExportEverything ->
            stringFn False fullTree

        ExportSubtree ->
            stringFn True activeTree

        ExportLeaves ->
            case exportFormat of
                JSON ->
                    treeToJSON False (Tree "0" "" (Children (getLeaves fullTree [])))
                        |> Enc.encode 2

                _ ->
                    getLeaves fullTree []
                        |> List.map .content
                        |> String.join "\n\n"

        ExportCurrentColumn ->
            case exportFormat of
                JSON ->
                    treeToJSON False (Tree "0" "" (Children currentColumnCards))
                        |> Enc.encode 2

                _ ->
                    currentColumnCards
                        |> List.map .content
                        |> String.join "\n\n"


toExtension : ExportFormat -> String
toExtension expFormat =
    case expFormat of
        DOCX ->
            "docx"

        JSON ->
            "json"

        OPML ->
            "opml"

        PlainText ->
            "md"



-- VIEW


exportView :
    { export : msg
    , printRequested : msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }
    -> String
    -> ( ExportSelection, ExportFormat )
    -> Tree
    -> Tree
    -> Html msg
exportView msgs docName (( _, exportFormat ) as exportSettings) activeTree fullTree =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }

        exportFormatString =
            case exportSettings |> Tuple.second of
                DOCX ->
                    DownloadWordFile

                PlainText ->
                    DownloadTextFile

                JSON ->
                    DownloadJSONFile

                OPML ->
                    DownloadOPMLFile

        actionButtons =
            div [ id "export-action-buttons" ]
                [ div
                    [ id "export-download"
                    , onClick msgs.export
                    , onMouseEnter <| msgs.tooltipRequested "export-download" BelowTooltip exportFormatString
                    , onMouseLeave msgs.tooltipClosed
                    ]
                    [ AntIcons.downloadOutlined [] ]
                , div
                    [ id "export-print"
                    , onClick msgs.printRequested
                    , onMouseEnter <| msgs.tooltipRequested "export-print" BelowLeftTooltip PrintThis
                    , onMouseLeave msgs.tooltipClosed
                    ]
                    [ AntIcons.printerOutlined [] ]
                ]
    in
    case exportFormat of
        DOCX ->
            div [ id "export-preview" ]
                [ Markdown.toHtmlWith options [ attribute "data-private" "lipsum" ] (toString docName exportSettings activeTree fullTree)
                , actionButtons
                ]

        _ ->
            div [ id "export-preview" ]
                [ div [ class "plain", attribute "data-private" "lipsum" ] [ text (toString docName exportSettings activeTree fullTree) ]
                , actionButtons
                ]


exportViewError : String -> Html never
exportViewError error =
    div [ id "export-preview" ] [ text error ]
