port module LegacyWorker exposing (..)

import Json.Decode as Dec
import Platform exposing (worker)


main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )



-- UPDATE


type Msg
    = Incoming ( String, Dec.Value )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming ( key, value ) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    input Incoming


port input : (( String, Dec.Value ) -> msg) -> Sub msg


port output : ( String, Dec.Value ) -> Cmd msg
