module Doc.Switcher exposing (..)

import Doc.List as DocList
import Doc.Metadata exposing (Metadata)
import Html exposing (Html, div, input)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onInput)


viewFileSwitcher : (String -> msg) -> Metadata -> String -> DocList.Model -> List (Html msg)
viewFileSwitcher searchInput currentDocument searchField docList =
    let
        filteredList =
            docList
                |> DocList.switchListSort currentDocument
                |> DocList.filter searchField
    in
    [ div [ class "modal-container" ]
        [ div [ class "modal-overlay" ] []
        , div [ id "switcher-modal" ]
            [ input
                [ id "switcher-input"
                , type_ "search"
                , value searchField
                , onInput searchInput
                , class "mousetrap"
                ]
                []
            , DocList.viewSwitcher currentDocument { docList = filteredList, selected = currentDocument }
            , div [ class "switcher-instructions" ]
                [--div [ class "switcher-instruction" ] [ span [ class "shortcut-key" ] [ text "↓ ↑" ], text " to navigate" ]
                ]
            ]
        ]
    ]
