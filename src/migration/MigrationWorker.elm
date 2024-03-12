port module MigrationWorker exposing (Model, Msg(..), init, update)

import Json.Decode as Dec
import Json.Encode as Enc
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
    = Incoming (Result Dec.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming (Ok value) ->
            ( model, output (Enc.int (value * value)) )

        Incoming (Err err) ->
            ( model
            , output (Enc.string (Dec.errorToString err))
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    input (Dec.decodeValue Dec.int >> Incoming)


port input : (Dec.Value -> msg) -> Sub msg


port output : Dec.Value -> Cmd msg
