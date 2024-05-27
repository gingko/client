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
    = Incoming ( String, Dec.Value )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming ( docId, json ) ->
            case Data.gitDataReceived json ( Data.empty, Tree.defaultTree ) of
                Just { newData, newTree } ->
                    let
                        converted =
                            Data.convert docId newData
                    in
                    case converted of
                        Just ( _, outvalue ) ->
                            ( model
                            , output ( docId, outvalue )
                            )

                        Nothing ->
                            ( model, output ( "ERROR", Enc.string "Conversion failed" ) )

                Nothing ->
                    ( model, output ( "ERROR", Enc.string "Invalid JSON" ) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    input Incoming


port input : (( String, Dec.Value ) -> msg) -> Sub msg


port output : ( String, Dec.Value ) -> Cmd msg
