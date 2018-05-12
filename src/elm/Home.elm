port module Home exposing (..)


import Html exposing (..)
import Html.Events exposing (..)
import Dict exposing (Dict)


main : Program Never Model Msg
main =
  program
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


init : ( Model, Cmd Msg )
init =
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

