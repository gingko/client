module Electron exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownOutline)
import Doc.TreeStructure as TreeStructure
import GlobalData
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode as Dec exposing (Decoder, Value)
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..), ParentMsg(..))
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (Theme(..), applyTheme)
import Task
import Translation exposing (TranslationId)
import Types exposing (TooltipPosition, Tree)


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
    { docModel : Page.Doc.Model
    , tooltip : Maybe ( Element, TooltipPosition, TranslationId )
    , theme : Theme
    }


init : ( Maybe String, Value ) -> ( Model, Cmd Msg )
init ( fileData_, json ) =
    let
        globalData =
            GlobalData.decode json

        initDocModel =
            case fileData_ of
                Nothing ->
                    Page.Doc.init True globalData

                Just fileData ->
                    case Coders.markdownOutlineHtmlParser fileData of
                        Ok (Just parsedTree) ->
                            Page.Doc.init False globalData |> initDoc parsedTree

                        Ok Nothing ->
                            let
                                _ =
                                    Debug.log "Nothing" "Nothing"
                            in
                            Page.Doc.init True globalData

                        Err err ->
                            let
                                _ =
                                    Debug.log "parse error" err
                            in
                            Page.Doc.init True globalData
    in
    ( { docModel = initDocModel, tooltip = Nothing, theme = Default }, Cmd.none )


initDoc : Tree -> Page.Doc.Model -> Page.Doc.Model
initDoc tree docModel =
    { docModel
        | workingTree = TreeStructure.setTree tree docModel.workingTree
        , loading = False
    }



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg
    | TooltipRequested String TooltipPosition TranslationId
    | TooltipReceived Element TooltipPosition TranslationId
    | TooltipClosed
    | Incoming Incoming.Msg
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ docModel } as model) =
    case msg of
        GotDocMsg docMsg ->
            let
                ( newDocModel, newCmd, parentMsg ) =
                    Page.Doc.update docMsg docModel
                        |> (\( m, c, p ) -> ( m, Cmd.map GotDocMsg c, p ))
            in
            case parentMsg of
                CloseTooltip ->
                    ( { model | docModel = newDocModel, tooltip = Nothing }, newCmd )

                LocalSaveDo saveTime ->
                    ( { model | docModel = newDocModel }, newCmd )
                        |> localSaveDo

                _ ->
                    ( { model | docModel = newDocModel }, newCmd )

        Incoming incomingMsg ->
            Page.Doc.incoming incomingMsg docModel
                |> Tuple.mapBoth (\m -> { model | docModel = m }) (Cmd.map GotDocMsg)

        LogErr err ->
            ( model, send (ConsoleLogRequested err) )

        TooltipRequested elId tipPos content ->
            ( model
            , Browser.Dom.getElement elId
                |> Task.attempt
                    (\result ->
                        case result of
                            Ok el ->
                                TooltipReceived el tipPos content

                            Err _ ->
                                NoOp
                    )
            )

        TooltipReceived el tipPos content ->
            ( { model | tooltip = Just ( el, tipPos, content ) }, Cmd.none )

        TooltipClosed ->
            ( { model | tooltip = Nothing }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


localSaveDo : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
localSaveDo ( model, prevCmd ) =
    ( model
    , Cmd.batch
        [ send <| SaveToFile (treeToMarkdownOutline False model.docModel.workingTree.tree)
        , prevCmd
        ]
    )



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ div [ id "app-root", applyTheme model.theme ]
        (Page.Doc.view
            { docMsg = GotDocMsg
            , keyboard = \s -> Incoming (Keyboard s)
            , tooltipRequested = TooltipRequested
            , tooltipClosed = TooltipClosed
            }
            model.docModel
        )
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Incoming.subscribe Incoming LogErr
        ]
