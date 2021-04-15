port module Page.Copy exposing (..)

-- MODEL

import Coders exposing (tupleDecoder)
import Doc.Data as Data
import Doc.Metadata as Metadata
import Doc.TreeStructure exposing (defaultTree)
import Http
import Import.Incoming
import Json.Decode as Dec
import Outgoing exposing (Msg(..), send)
import RandomId
import Route
import Session exposing (Session)
import Types exposing (Tree)


type alias Model =
    { session : Session
    , tree : Maybe Tree
    }


init : Session -> String -> ( Model, Cmd Msg )
init user dbName =
    ( { session = user, tree = Nothing }, send <| CopyDocument dbName )



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
                    Data.received dataIn ( Data.empty, defaultTree ) |> Maybe.map .newTree
            in
            case newTree_ of
                Just newTree ->
                    ( model
                    , RandomId.generate (IdGenerated newTree newTitle)
                    )

                Nothing ->
                    ( model, Route.replaceUrl (Session.navKey model.session) Route.Root )

        IdGenerated tree fileName docId ->
            let
                author =
                    model.session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

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
                    ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

                Nothing ->
                    ( model, Route.replaceUrl (Session.navKey model.session) Route.Root )


toUser : Model -> Session
toUser model =
    model.session



-- SUBSCRIPTIONS


port copyLoaded : (Dec.Value -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ copyLoaded CopyLoaded
        , Import.Incoming.importComplete CopySaved
        ]
