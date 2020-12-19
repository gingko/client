module Doc.Switcher exposing (Model, down, view)

import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, div, input)
import Html.Attributes exposing (class, id, type_, value)
import Html.Events exposing (onInput)
import List.Extra as ListExtra



-- MODEL


type alias Model =
    { currentDocument : Metadata
    , selectedDocument : Maybe String
    , searchField : String
    , docList : DocList.Model
    }



-- UPDATE


down : Model -> Model
down ({ currentDocument, selectedDocument, searchField, docList } as model) =
    let
        filteredDocs =
            docList
                |> DocList.switchListSort currentDocument
                |> DocList.filter searchField
                |> DocList.toList
    in
    case selectedDocument of
        Just selected ->
            let
                newSel =
                    case filteredDocs of
                        Just docs ->
                            docs
                                |> List.map (\md -> ( md, selected == Metadata.getDocId md ))
                                |> ListExtra.takeWhileRight (not << Tuple.second)
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.map Metadata.getDocId
                                |> Maybe.withDefault selected

                        Nothing ->
                            selected
            in
            { model | selectedDocument = Just newSel }

        Nothing ->
            { model
                | selectedDocument =
                    filteredDocs
                        |> Maybe.andThen List.head
                        |> Maybe.map Metadata.getDocId
            }



-- VIEW


view : (String -> msg) -> Model -> List (Html msg)
view searchInput { currentDocument, selectedDocument, searchField, docList } =
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
            , DocList.viewSwitcher currentDocument
                { docList = filteredList
                , selected = selectedDocument |> Maybe.withDefault (Metadata.getDocId currentDocument)
                }
            , div [ class "switcher-instructions" ]
                [--div [ class "switcher-instruction" ] [ span [ class "shortcut-key" ] [ text "↓ ↑" ], text " to navigate" ]
                ]
            ]
        ]
    ]
