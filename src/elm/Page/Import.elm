module Page.Import exposing (..)

-- MODEL

import Doc.Data as Data
import Doc.Metadata as Metadata
import Http
import Import.Incoming
import Import.Single
import Import.Template as Template exposing (Template)
import Outgoing exposing (Msg(..), send)
import RandomId
import Route
import Session exposing (Session)
import Types exposing (Tree)


type alias Model =
    Session


init : Session -> Template -> ( Model, Cmd Msg )
init user template =
    let
        ( importTreeDecoder, newSeed ) =
            Import.Single.decoder (Session.seed user)
    in
    ( Session.setSeed newSeed user, Template.fetchJSON (TemplateJSONReceived (Template.toString template)) importTreeDecoder template )



-- UPDATE


type Msg
    = TemplateJSONReceived String (Result Http.Error Tree)
    | TemplateImported Tree String String
    | TemplateImportSaved (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg user =
    case msg of
        TemplateJSONReceived fileName result ->
            case result of
                Ok tree ->
                    ( user
                    , RandomId.generate (TemplateImported tree fileName)
                    )

                Err _ ->
                    ( user, Route.replaceUrl (Session.navKey user) Route.Root )

        TemplateImported tree fileName docId ->
            let
                author =
                    user |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> Metadata.renameAndEncode fileName)
            in
            case commitReq_ of
                Just commitReq ->
                    ( user, send <| SaveImportedData commitReq )

                Nothing ->
                    ( user, Cmd.none )

        TemplateImportSaved docId_ ->
            case docId_ of
                Just docId ->
                    ( user, Route.pushUrl (Session.navKey user) (Route.DocUntitled docId) )

                Nothing ->
                    ( user, Route.replaceUrl (Session.navKey user) Route.Root )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Import.Incoming.importComplete TemplateImportSaved
