port module Electron exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, button, h1, text)
import Json.Decode as Dec exposing (Decoder, Value)
import Page.Doc exposing (Msg(..))
import Session exposing (Session)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Title" (view m)
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    Page.Doc.Model


init : Value -> Nav.Key -> ( Model, Cmd Msg )
init json navKey =
    let
        session =
            Session.decode navKey json
    in
    ( Page.Doc.init True session "randDocId", Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> List (Html Msg)
view model =
    Page.Doc.view
        { docMsg = GotDocMsg
        , keyboard = always NoOp
        , tooltipRequested = always <| always <| always NoOp
        , tooltipClosed = NoOp
        , toggleWordcount = always NoOp
        , toggleUpgradeModal = always NoOp
        }
        model



-- PORTS


port send : () -> Cmd msg
