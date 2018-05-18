port module Home exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Dict exposing (Dict)
import Json.Encode as Json exposing (string, null)
import Json.Decode exposing (succeed)
import Coders exposing (maybeToValue)
import Octicons as Icon


main : Program ( List (String, Document) ) Model Msg
main =
  programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }




-- MODEL


type alias Model =
  { documents : Dict String Document
  , archiveDropdown : Bool
  }


type alias Document =
  { name : Maybe String
  , state : String
  , created_at : String
  , last_modified : String
  }


defaultDocument : Document
defaultDocument =
  { name = Just "Untitled"
  , state = "active"
  , created_at = ""
  , last_modified = ""
  }


init : List (String, Document) -> ( Model, Cmd Msg )
init dbObj =
  { documents = dbObj |> Dict.fromList
  , archiveDropdown = False
  }
  ! []




-- UPDATE

type Msg
  = NoOp
  | New
  | Import
  | Load String (Maybe String)
  | SetState String String
  | Delete String
  | ToggleArchive



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    New ->
      model ! [ forJS { tag = "New", data = null }]

    Import ->
      model ! [ forJS { tag = "ImportGko", data = null } ]

    Load dbname docName_ ->
      let
        data =
          [ string dbname, maybeToValue string docName_ ]
            |> Json.list
      in
      model ! [ forJS { tag = "Load", data = data }]

    SetState dbname state ->
      let
        data = Json.list [ string dbname, string state ]
      in
      { model
        | documents =
            model.documents
              |> Dict.update dbname ( Maybe.map (\v -> { v | state = state }) )
      }
        ! [ forJS { tag = "SetState", data = data } ]

    Delete dbname ->
      { model
        | documents =
            model.documents
              |> Dict.filter ( \k _ -> k /= dbname )
      }
        ! [ forJS { tag = "Delete", data = string dbname } ]

    ToggleArchive ->
      if ( model.documents |> Dict.filter (\_ v -> v.state == "archived") |> Dict.size ) /= 0 then
        ( { model | archiveDropdown = not model.archiveDropdown }, Cmd.none )
      else
        model ! []

    NoOp ->
      model ! []




-- VIEW

view : Model -> Html Msg
view {documents, archiveDropdown} =
  let
    visibleWhen bool =
      classList [("visible", bool), ("hidden", not bool)]

    numActive =
      documents
      |> Dict.filter (\_ v -> v.state == "active")
      |> Dict.size

    numArchived =
      documents
      |> Dict.filter (\_ v -> v.state == "archived")
      |> Dict.size

    archivedText bool =
      "Archived (" ++ ( numArchived |> toString) ++ ")"
      ++ ( case (bool, numArchived == 0) of
            (_, True) -> ""
            (True, _) -> " ▴"
            (False, _) -> " ▾"
         )
  in
  div []
    [ div [ id "template-block" ]
        [ div [ class "template-item", onClick New ]
            [ div [ classList [("template-thumbnail", True), ("new", True)]][]
            , div [ class "template-title"][ text "Blank" ]
            ]
        , div [ class "template-item", onClick Import ]
            [ div [ classList [("template-thumbnail", True), ("import", True)]][ Icon.file ( Icon.defaultOptions |> Icon.size 48) ]
            , div [ class "template-title"][ text "Import From File" ]
            ]
        ]
    , div [ id "documents-block" ]
        [ div
            [ class "list-header", visibleWhen ( numActive /= 0 )  ]
            [ div [][ text "Name" ]
            , div [][ text "Date Modified" ]
            ]
        , viewDocList "active" documents
        , h4 [ onClick ToggleArchive, class "list-section-header" ][text <| archivedText archiveDropdown ]
        , div [ visibleWhen ( archiveDropdown && numArchived /= 0 ) ]
            [ div
              [ class "list-header" ]
              [ div [][ text "Name" ]
              , div [][ text "Date Modified" ]
              ]
            , viewDocList "archived" documents
            ]
        ]
    ]


viewDocList : String -> Dict String Document -> Html Msg
viewDocList state docDict =
  div [ class "document-list" ]
    ( docDict
      |> Dict.filter (\k v -> v.state == state)
      |> Dict.toList
      |> List.sortBy ( \(k, v) -> v.last_modified )
      |> List.reverse
      |> List.map viewDocumentItem
    )


viewDocumentItem : ( String, Document) -> Html Msg
viewDocumentItem (dbname, document) =
  let
    onClickThis msg =
      onWithOptions "click" { defaultOptions | stopPropagation = True } (succeed msg)

    buttons =
      case document.state of
        "archived" ->
          [ div
              [ onClickThis (Delete dbname), title "Delete document"]
              [ Icon.trashcan Icon.defaultOptions ]
          , div
              [ onClickThis (SetState dbname "active"), title "Restore document"]
              [ Icon.arrowUp Icon.defaultOptions ]
          ]

        _ ->
          [ div
              [ onClickThis (SetState dbname "archived"), title "Archive document"]
              [ Icon.archive Icon.defaultOptions ]
          ]

  in
  div
    [ class "document-item", onClick (Load dbname document.name) ]
    [ div [ class "doc-title" ][ text ( document.name |> Maybe.withDefault "Untitled" ) ]
    , div [ class "doc-modified" ][ text document.last_modified ]
    , div [ class "doc-buttons" ] buttons
    ]


-- SUBSCRIPTIONS

port forJS : { tag : String, data : Json.Value } -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

