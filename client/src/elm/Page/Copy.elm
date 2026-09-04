port module Page.Copy exposing (..)

-- MODEL

import Browser.Navigation as Nav
import Coders exposing (tupleDecoder)
import Doc.Data as Data
import Doc.Metadata as Metadata
import Doc.TreeStructure exposing (defaultTree)
import GlobalData exposing (GlobalData)
import Import.Incoming
import Json.Decode as Dec
import Outgoing exposing (Msg(..), send)
import RandomId
import Route
import Session exposing (LoggedIn, Session(..))
import Types exposing (Tree)


type alias Model =
    { globalData : GlobalData
    , session : LoggedIn
    , tree : Maybe Tree
    , navKey : Nav.Key
    }


init : Nav.Key -> GlobalData -> LoggedIn -> String -> ( Model, Cmd Msg )
init nKey gData session dbName =
    ( { globalData = gData
      , session = session
      , tree = Nothing
      , navKey = nKey
      }
    , send <| CopyDocument dbName
    )


navKey : Model -> Nav.Key
navKey model =
    model.navKey


globalData : Model -> GlobalData
globalData model =
    model.globalData



-- UPDATE


type Msg
    = CopyLoaded Dec.Value
    | IdGenerated Tree String String
    | CopySaved (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CopyLoaded dataTuple ->
            let
                ( newTitle, dataIn ) =
                    case Dec.decodeValue (tupleDecoder Dec.string Dec.value) dataTuple of
                        Ok ( title, dat ) ->
                            ( title ++ " (Copy)", dat )

                        Err _ ->
                            ( "", dataTuple )

                newTree_ =
                    Data.gitDataReceived dataIn ( Data.empty, defaultTree ) |> Maybe.map .newTree
            in
            case newTree_ of
                Just newTree ->
                    ( model
                    , RandomId.generate (IdGenerated newTree newTitle)
                    )

                Nothing ->
                    ( model, Route.replaceUrl model.navKey Route.Root )

        IdGenerated tree fileName docId ->
            let
                author =
                    model.session |> Session.name

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> Metadata.renameAndEncode fileName)
            in
            case commitReq_ of
                Just commitReq ->
                    ( model, send <| SaveImportedData commitReq )

                Nothing ->
                    ( model, Cmd.none )

        CopySaved docId_ ->
            case docId_ of
                Just docId ->
                    ( model, Route.pushUrl model.navKey (Route.DocUntitled docId) )

                Nothing ->
                    ( model, Route.replaceUrl model.navKey Route.Root )


toSession : Model -> Session
toSession model =
    model.session |> LoggedInSession



-- SUBSCRIPTIONS


port copyLoaded : (Dec.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ copyLoaded CopyLoaded
        , Import.Incoming.importComplete CopySaved
        ]
