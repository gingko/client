port module Home exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (id, class)
import Dict exposing (Dict)
import Json.Encode as Json exposing (..)
import Coders exposing (maybeToValue)


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
  Dict String Document


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
  ( dbObj
      |> Dict.fromList
  , Cmd.none
  )




-- UPDATE

type Msg
  = NoOp
  | New
  | Import
  | Load String (Maybe String)
  | SetState String String
  | Delete String



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
            |> list
      in
      model ! [ forJS { tag = "Load", data = data }]

    SetState dbname state ->
      let
        data = list [ string dbname, string state ]
      in
      ( model
          |> Dict.update dbname ( Maybe.map (\v -> { v | state = state }) )
      )
        ! [ forJS { tag = "SetState", data = data } ]

    Delete dbname ->
      ( model
          |> Dict.filter ( \k _ -> k /= dbname )
      )
        ! [ forJS { tag = "Delete", data = string dbname } ]

    _ ->
      model ! []




-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ div [ id "template-block" ]
        [ button [ onClick New ][ text "New" ]
        , button [ onClick Import ][ text "Import *.gko file" ]
        ]
    , div [ id "documents-block" ]
        [ div
            [ class "list-header" ]
            [ div [][ text "Name" ]
            , div [][ text "Date Modified" ]
            ]
        , viewDocList "active" model
        , h3 [][ text "Archived"]
        , viewDocList "archived" model
        ]
    ]


viewDocList : String -> Model -> Html Msg
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
    buttons =
      case document.state of
        "archived" ->
          [ button [ onClick (Delete dbname)][ text "Delete" ]
          , button [ onClick (SetState dbname "active")][ text "Restore" ]
          ]

        _ ->
          [ button [onClick (Load dbname document.name)][ text "Open" ]
          , button [ onClick (SetState dbname "archived")][ text "Archive" ]
          ]
  in
  div
    [ class "document-item", onClick (Load dbname document.name) ]
    [ div [ class "doc-title" ][ text ( document.name |> Maybe.withDefault "Untitled" ) ]
    , div [ class "doc-modified" ][ text document.last_modified ]
    ]


-- SUBSCRIPTIONS

port forJS : { tag : String, data : Json.Value } -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

