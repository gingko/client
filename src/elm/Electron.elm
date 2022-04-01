module Electron exposing (..)

import Browser
import Coders
import Doc.TreeStructure as TreeStructure
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode as Dec exposing (Decoder, Value)
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..))
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (applyTheme)
import Session exposing (Session)
import Types exposing (Tree)


main : Program ( Maybe String, Value ) Model Msg
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


init : ( Maybe String, Value ) -> ( Model, Cmd Msg )
init ( fileData_, json ) =
    let
        session =
            Session.decode json
    in
    case fileData_ of
        Nothing ->
            ( Page.Doc.init True session "randDocId"
                |> setDesktop True
            , Cmd.none
            )

        Just fileData ->
            case Coders.markdownOutlineHtmlParser fileData of
                Ok (Just parsedTree) ->
                    ( Page.Doc.init False session "randDocId"
                        |> initDoc parsedTree
                        |> setDesktop True
                    , Cmd.none
                    )

                Ok Nothing ->
                    let
                        _ =
                            Debug.log "Nothing" "Nothing"
                    in
                    ( Page.Doc.init True session "randDocId"
                        |> setDesktop True
                    , Cmd.none
                    )

                Err err ->
                    let
                        _ =
                            Debug.log "parse error" err
                    in
                    ( Page.Doc.init True session "randDocId"
                        |> setDesktop True
                    , Cmd.none
                    )


initDoc : Tree -> Page.Doc.Model -> Page.Doc.Model
initDoc tree docModel =
    { docModel
        | workingTree = TreeStructure.setTree tree docModel.workingTree
        , loading = False
    }


setDesktop : Bool -> Page.Doc.Model -> Page.Doc.Model
setDesktop isDesktop docModel =
    { docModel | isDesktop = isDesktop }



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
            Page.Doc.updateDoc docMsg doc
                |> Tuple.mapSecond (Cmd.map GotDocMsg)

        Incoming incomingMsg ->
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
