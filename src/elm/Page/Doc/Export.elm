module Page.Doc.Export exposing (ExportFormat(..), ExportSelection(..), command, exportView, exportViewError)

import Api
import Bytes exposing (Bytes)
import Coders exposing (treeToJSON, treeToMarkdownString)
import File.Download as Download
import Html exposing (Html, div, pre, text)
import Html.Attributes exposing (class, id)
import Http
import Json.Encode as Enc
import Markdown
import Types exposing (Tree)


type ExportSelection
    = ExportEverything
    | ExportSubtree
    | ExportColumn Int


type ExportFormat
    = PlainText
    | DOCX
    | JSON


command : (String -> Result Http.Error Bytes -> msg) -> String -> String -> ( ExportSelection, ExportFormat ) -> Tree -> Tree -> Cmd msg
command exportedMsg docId docName (( _, exportFormat ) as exportSettings) activeTree fullTree =
    case exportFormat of
        JSON ->
            Download.string (docName ++ ".json") "application/json" (toString exportSettings activeTree fullTree)

        PlainText ->
            Download.string (docName ++ ".txt") "text/plain" (toString exportSettings activeTree fullTree)

        DOCX ->
            Api.exportDocx
                (exportedMsg docName)
                { docId = docId, markdown = toString exportSettings activeTree fullTree }


toString : ( ExportSelection, ExportFormat ) -> Tree -> Tree -> String
toString ( exportSelection, exportFormat ) activeTree fullTree =
    let
        stringFn withRoot tree =
            case exportFormat of
                JSON ->
                    treeToJSON withRoot tree
                        |> Enc.encode 2

                _ ->
                    treeToMarkdownString withRoot tree
    in
    case exportSelection of
        ExportEverything ->
            stringFn False fullTree

        ExportSubtree ->
            stringFn True activeTree

        _ ->
            stringFn True activeTree



-- VIEW


exportView : ( ExportSelection, ExportFormat ) -> Tree -> Tree -> Html never
exportView (( _, exportFormat ) as exportSettings) activeTree fullTree =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }
    in
    case exportFormat of
        DOCX ->
            div [ id "export-preview" ]
                [ div [ class "top-buffer" ] []
                , Markdown.toHtmlWith options [] (toString exportSettings activeTree fullTree)
                ]

        _ ->
            div [ id "export-preview" ]
                [ div [ class "top-buffer" ] []
                , pre [] [ text (toString exportSettings activeTree fullTree) ]
                ]


exportViewError : String -> Html never
exportViewError error =
    div [ id "export-preview" ] [ text error ]
