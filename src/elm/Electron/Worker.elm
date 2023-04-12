port module Electron.Worker exposing (..)

import Coders exposing (treeToMarkdownOutline)
import Import.Single
import Json.Decode as Dec
import Json.Encode as Enc exposing (Value)
import Platform exposing (worker)
import Random


main : Platform.Program Int Model Msg
main =
    worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Random.Seed


init : Int -> ( Model, Cmd Msg )
init initSeed =
    ( Random.initialSeed initSeed, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | ImportStringReceived ( String, Bool )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg seed =
    case msg of
        ImportStringReceived ( impString, fromHomePage ) ->
            let
                ( importTreeDecoder, newSeed ) =
                    Import.Single.decoder seed
            in
            case Dec.decodeString importTreeDecoder impString of
                Ok treeImported ->
                    let
                        stringToSave =
                            treeToMarkdownOutline False treeImported
                    in
                    ( newSeed
                    , fromElm
                        ( "ImportDone"
                        , Enc.object
                            [ ( "data", Enc.string stringToSave )
                            , ( "fromHomePage", Enc.bool fromHomePage )
                            ]
                        )
                    )

                Err err ->
                    ( newSeed, fromElm ( "ImportError", Dec.errorToString err |> Enc.string ) )

        NoOp ->
            ( seed, Cmd.none )



-- PORTS


port fromElm : ( String, Value ) -> Cmd msg


port toElm : (( String, Bool ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    toElm ImportStringReceived
