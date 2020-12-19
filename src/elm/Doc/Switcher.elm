module Doc.Switcher exposing (Model, view)

import Doc.List as DocList
import Doc.Metadata exposing (Metadata)
import Html exposing (Html, div, input)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onInput)


type alias Model =
    { currentDocument : Metadata
    , selectedDocument : Maybe String
    , searchField : String
    , docList : DocList.Model
    }


view : (String -> msg) -> Model -> List (Html msg)
view searchInput { currentDocument, searchField, docList } =
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
