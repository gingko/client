module Page.Import exposing (..)

-- MODEL

import Browser.Navigation as Nav
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
    { session : Session
    , navKey : Nav.Key
    }


init : Nav.Key -> Session -> Template -> ( Model, Cmd Msg )
init navKey user template =
    let
        ( importTreeDecoder, newSeed ) =
            Import.Single.decoder (Session.seed user)
    in
    ( { session = Session.setSeed newSeed user, navKey = navKey }
    , Template.fetchJSON (TemplateJSONReceived (Template.toString template)) importTreeDecoder template
    )



-- UPDATE


type Msg
    = TemplateJSONReceived String (Result Http.Error Tree)
    | TemplateImported Tree String String
    | TemplateImportSaved (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemplateJSONReceived fileName result ->
            case result of
                Ok tree ->
                    ( model
                    , RandomId.generate (TemplateImported tree fileName)
                    )

                Err _ ->
                    ( model, Route.replaceUrl model.navKey Route.Root )

        TemplateImported tree fileName docId ->
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

        TemplateImportSaved docId_ ->
            case docId_ of
                Just docId ->
                    ( model, Route.pushUrl model.navKey (Route.DocUntitled docId) )

                Nothing ->
                    ( model, Route.replaceUrl model.navKey Route.Root )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Import.Incoming.importComplete TemplateImportSaved
