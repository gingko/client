module Doc.Switcher exposing (Model, down, search, up, view)

import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, div, input, span, text)
import Html.Attributes exposing (class, id, placeholder, type_, value)
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
    case selectedDocument of
        Just selected ->
            let
                newSel =
                    case filteredDocs model of
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
                    filteredDocs model
                        |> Maybe.andThen List.head
                        |> Maybe.map Metadata.getDocId
            }


up : Model -> Model
up ({ currentDocument, selectedDocument, searchField, docList } as model) =
    case selectedDocument of
        Just selected ->
            let
                newSel =
                    case filteredDocs model of
                        Just docs ->
                            docs
                                |> List.map (\md -> ( md, selected == Metadata.getDocId md ))
                                |> ListExtra.takeWhile (not << Tuple.second)
                                |> List.reverse
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
                    filteredDocs model
                        |> Maybe.andThen List.head
                        |> Maybe.map Metadata.getDocId
            }


search : String -> Model -> Model
search term model =
    let
        newModel =
            { model | searchField = term }
    in
    { newModel
        | selectedDocument =
            filteredDocs newModel
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
                , placeholder "Type file name to select"
                ]
                []
            , DocList.viewSwitcher currentDocument
                { docList = filteredList
                , selected = selectedDocument |> Maybe.withDefault (Metadata.getDocId currentDocument)
                }
            , div [ class "switcher-instructions" ]
                [ div [ class "switcher-instruction" ] [ span [ class "shortcut-key" ] [ text "↓ ↑" ], text " to select" ]
                , div [ class "switcher-instruction" ] [ span [ class "shortcut-key" ] [ text "Enter" ], text " to open" ]
                , div [ class "switcher-instruction" ] [ span [ class "shortcut-key" ] [ text "Esc" ], text " to dismiss" ]
                ]
            ]
        ]
    ]



-- HELPERS


filteredDocs : Model -> Maybe (List Metadata)
filteredDocs { docList, currentDocument, searchField } =
    docList
        |> DocList.switchListSort currentDocument
        |> DocList.filter searchField
        |> DocList.toList
