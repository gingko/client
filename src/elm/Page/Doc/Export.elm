module Page.Doc.Export exposing (..)

import Coders exposing (treeToMarkdownString)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id)
import Markdown
import Types exposing (Tree)


exportView : Tree -> Html never
exportView tree =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }
    in
    div [ id "export-preview" ]
        [ div [ class "top-buffer" ] []
        , Markdown.toHtmlWith options [] (treeToMarkdownString False tree)
        ]
