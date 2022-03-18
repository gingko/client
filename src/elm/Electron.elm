module Electron exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode as Dec exposing (Decoder, Value)
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..))
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (applyTheme)
import Session exposing (Session)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Title" (view m)
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Page.Doc.Model


init : Value -> ( Model, Cmd Msg )
init json =
    let
        session =
            Session.decode json
    in
    ( Page.Doc.init True session "randDocId", Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg
    | Incoming Incoming.Msg
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg doc =
    case msg of
        GotDocMsg docMsg ->
            Page.Doc.update docMsg doc
                |> Tuple.mapSecond (Cmd.map GotDocMsg)

        Incoming incomingMsg ->
            let
                _ =
                    Debug.log "incoming" incomingMsg
            in
            Page.Doc.incoming incomingMsg doc
                |> Tuple.mapSecond (Cmd.map GotDocMsg)

        LogErr err ->
            ( doc
            , send (ConsoleLogRequested err)
            )

        NoOp ->
            ( doc, Cmd.none )



-- VIEW


view : Model -> List (Html Msg)
view doc =
    [ div [ id "app-root", applyTheme doc.theme ]
        (Page.Doc.view
            { docMsg = GotDocMsg
            , keyboard = \s -> Incoming (Keyboard s)
            , tooltipRequested = always <| always <| always NoOp
            , tooltipClosed = NoOp
            , toggleWordcount = always NoOp
            , toggleUpgradeModal = always NoOp
            }
            doc
        )
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Incoming.subscribe Incoming LogErr
        ]
