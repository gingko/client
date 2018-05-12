port module Home exposing (..)


import Html exposing (..)
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
  model ! []




-- VIEW

view : Model -> Html Msg
view model =
  h2 [][ text "Hi from Elm" ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

