port module Home exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json


main : Program ( List String ) Model Msg
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
  { name : String
  , state : String
  , created_at : String
  , last_modified : String
  }


defaultDocument : Document
defaultDocument =
  { name = "Untitled"
  , state = "active"
  , created_at = ""
  , last_modified = ""
  }


init : List String -> ( Model, Cmd Msg )
init dbs =
  ( dbs
      |> List.map ( \db -> ( db, defaultDocument ) )
      |> Dict.fromList
  , Cmd.none
  )




-- UPDATE

type Msg
  = NoOp
  | New
  | Load String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    New ->
      model ! [ forJS { tag = "New", data = "" }]

    Load dbname ->
      model ! [ forJS { tag = "Load", data = dbname }]

    _ ->
      model ! []




-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick New ][ text "New" ]
    , viewDocList model
    ]


viewDocList : Model -> Html Msg
viewDocList docDict =
  ul []
    ( docDict
      |> Dict.toList
      |> List.map (\(k, v) -> li [][ button [ onClick ( Load k ) ] [ text k ] ])
    )


-- SUBSCRIPTIONS

port forJS : { tag : String, data : String } -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

