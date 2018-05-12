port module Home exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)
import Json.Decode as Json


main : Program Json.Value Model Msg
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


init : Json.Value -> ( Model, Cmd Msg )
init json =
  let _ = Debug.log "json" json in
  Dict.empty
   ! []




-- UPDATE

type Msg
  = NoOp
  | New
  | Open String



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    New ->
      model ! [ forJS "New"]

    _ ->
      model ! []




-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ text "Hi from Elm" 
    , button [ onClick New ][ text "New" ]
    ]



-- SUBSCRIPTIONS

port forJS : String -> Cmd msg

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

