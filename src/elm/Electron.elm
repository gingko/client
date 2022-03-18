port module Electron exposing (..)

import Browser
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode as Dec exposing (Decoder, Value)
import Page.Doc exposing (Msg(..))
import Page.Doc.Theme exposing (applyTheme)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg doc =
    case msg of
        GotDocMsg docMsg ->
            Page.Doc.update docMsg doc |> Tuple.mapSecond (Cmd.map GotDocMsg)

        NoOp ->
            ( doc, Cmd.none )



-- VIEW


view : Model -> List (Html Msg)
view doc =
    [ div [ id "app-root", applyTheme doc.theme ]
        (Page.Doc.view
            { docMsg = GotDocMsg
            , keyboard = always NoOp
            , tooltipRequested = always <| always <| always NoOp
            , tooltipClosed = NoOp
            , toggleWordcount = always NoOp
            , toggleUpgradeModal = always NoOp
            }
            doc
        )
    ]



-- PORTS


port send : () -> Cmd msg
