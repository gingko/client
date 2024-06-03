port module LegacyWorker exposing (..)

import Coders exposing (treeToJSON)
import Doc.Data as Data
import Doc.TreeStructure as TreeStructure
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


type alias InputData =
    { email : String, name : String, treeId : String, cards : Dec.Value }


type alias OutputData =
    { email : String, name : String, treeId : String, treeJSON : Dec.Value }



-- UPDATE


type Msg
    = Incoming InputData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Incoming { email, name, treeId, cards } ->
            case Data.cardDataReceived cards ( Data.empty, TreeStructure.defaultTree, treeId ) of
                Just { newTree } ->
                    ( (), output { email = email, name = name, treeId = treeId, treeJSON = treeToJSON False newTree } )

                Nothing ->
                    ( (), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    input Incoming


port input : (InputData -> msg) -> Sub msg


port output : OutputData -> Cmd msg
