module Page.Import exposing (..)

-- MODEL

import Http
import Import.Incoming
import Import.Single
import Import.Template as Template exposing (Template)
import Outgoing exposing (Msg(..), send)
import RandomId
import Route
import Types exposing (Tree)
import User exposing (User)


type alias Model =
    User


init : User -> Template -> ( Model, Cmd Msg )
init user template =
    let
        ( importTreeDecoder, newSeed ) =
            Import.Single.decoder (User.seed user)
    in
    ( User.setSeed newSeed user, Template.fetchJSON (TemplateJSONReceived (Template.toString template)) importTreeDecoder template )



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
                    ( user, Route.replaceUrl (User.navKey user) Route.Root )

        TemplateImported tree fileName docId ->
            let
                author =
                    user |> User.name |> Maybe.withDefault "jane.doe@gmail.com"
            in
            ( user, send <| SaveImportedData (Import.Single.encode { author = author, docId = docId, fileName = fileName } tree) )

        TemplateImportSaved docId_ ->
            case docId_ of
                Just docId ->
                    ( user, Route.pushUrl (User.navKey user) (Route.DocUntitled docId) )

                Nothing ->
                    ( user, Route.replaceUrl (User.navKey user) Route.Root )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Import.Incoming.importComplete TemplateImportSaved
