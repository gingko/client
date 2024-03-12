port module MigrationWorker exposing (Model, Msg(..), init, update)

import Doc.Data as Data
import Doc.TreeStructure as Tree
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
    = Incoming Dec.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming json ->
            case Data.gitDataReceived json ( Data.empty, Tree.defaultTree ) of
                Just { newData, newTree } ->
                    ( model
                    , output
                        (Debug.toString newTree
                            |> Enc.string
                        )
                    )

                Nothing ->
                    ( model, output (Enc.string "Invalid JSON") )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    input Incoming


port input : (Dec.Value -> msg) -> Sub msg


port output : Dec.Value -> Cmd msg
