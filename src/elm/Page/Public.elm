module Page.Public exposing (Model, Msg, init, navKey, subscriptions, update, view)

import Api
import Browser.Navigation as Nav
import Doc.Data as Data
import GlobalData exposing (GlobalData)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, id)
import Http
import Outgoing exposing (Msg(..), send)
import Page.Doc
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Translation exposing (TranslationId)
import Types exposing (TooltipPosition, Tree)



-- MODEL


type alias Model =
    { title : String
    , loading : Bool
    , navKey : Nav.Key
    , doc : Page.Doc.Model
    }


init : Nav.Key -> GlobalData -> String -> ( Model, Cmd Msg )
init key globalData dbName =
    ( { title = "Public Page"
      , loading = True
      , navKey = key
      , doc = Page.Doc.init False globalData
      }
    , getPublicDocument dbName
    )



-- GETTERS


navKey : Model -> Nav.Key
navKey model =
    model.navKey



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg
    | IncomingDocMsg Incoming.Msg
    | TooltipRequested String TooltipPosition TranslationId
    | TooltipClosed
    | DataReceived (Result Http.Error Tree)
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotDocMsg docMsg ->
            let
                ( newDocModel, newCmd, parentMsgs ) =
                    Page.Doc.opaqueUpdate docMsg model.doc
                        |> (\( m, c, p ) -> ( m, Cmd.map GotDocMsg c, p ))
            in
            ( { model | doc = newDocModel }, newCmd )

        IncomingDocMsg incomingMsg ->
            let
                ( newDocModel, newCmd, parentMsgs ) =
                    Page.Doc.opaqueIncoming incomingMsg model.doc
                        |> (\( m, c, p ) -> ( m, Cmd.map GotDocMsg c, p ))
            in
            ( { model | doc = newDocModel }, newCmd )

        TooltipRequested _ _ _ ->
            ( model, Cmd.none )

        TooltipClosed ->
            ( model, Cmd.none )

        DataReceived (Ok tree) ->
            ( { model
                | doc = Page.Doc.publicTreeLoaded tree model.doc
                , loading = False
              }
            , Cmd.none
            )

        DataReceived (Err _) ->
            ( model, Cmd.none )

        LogErr err ->
            ( model
            , send (ConsoleLogRequested err)
            )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app-root", classList [ ( "loading", model.loading ) ] ]
        (Page.Doc.view
            { docMsg = GotDocMsg
            , keyboard = \s -> IncomingDocMsg (Keyboard s)
            , tooltipRequested = TooltipRequested
            , tooltipClosed = TooltipClosed
            }
            Nothing
            Nothing
            model.doc
        )



-- REQUESTS


getPublicDocument : String -> Cmd Msg
getPublicDocument dbName =
    Api.getPublicDocument DataReceived Data.publicDataDecoder dbName



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Incoming.subscribe IncomingDocMsg LogErr
