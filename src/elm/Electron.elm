module Electron exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownOutline)
import Doc.Data as Data
import Doc.TreeStructure as TreeStructure exposing (Msg(..))
import GlobalData
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode exposing (Decoder, Value)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..), ParentMsg(..))
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (Theme(..), applyTheme)
import Task
import Translation exposing (TranslationId)
import Types exposing (TooltipPosition, Tree, ViewMode(..))


main : Program ( String, Maybe String, Value ) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Gingko Writer Desktop" (view m)
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { docModel : Page.Doc.Model
    , fileState : FileState
    , tooltip : Maybe ( Element, TooltipPosition, TranslationId )
    , theme : Theme
    }


type FileState
    = UntitledFileDoc String
    | FileDoc String


fileStateToPath : FileState -> String
fileStateToPath fState =
    case fState of
        UntitledFileDoc str ->
            str

        FileDoc str ->
            str


init : ( String, Maybe String, Value ) -> ( Model, Cmd Msg )
init ( filename, fileData_, json ) =
    let
        globalData =
            GlobalData.decode json

        ( initDocModel, initFileState ) =
            case fileData_ of
                Nothing ->
                    ( Page.Doc.init True globalData
                    , UntitledFileDoc filename
                    )

                Just fileData ->
                    case Coders.markdownOutlineHtmlParser fileData of
                        Ok (Just parsedTree) ->
                            ( Page.Doc.init False globalData |> initDoc parsedTree
                            , FileDoc filename
                            )

                        Ok Nothing ->
                            ( Page.Doc.init True globalData
                            , UntitledFileDoc "Nothing error"
                            )

                        Err err ->
                            ( Page.Doc.init True globalData
                            , UntitledFileDoc "parser error"
                            )
    in
    ( { docModel = initDocModel
      , fileState = initFileState
      , tooltip = Nothing
      , theme = Default
      }
    , Cmd.none
    )


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

                CommitDo commitTime ->
                    ( { model | docModel = newDocModel }, newCmd )
                        |> addToHistoryDo

                _ ->
                    ( { model | docModel = newDocModel }, newCmd )

        Incoming incomingMsg ->
            case ( model.fileState, incomingMsg ) of
                ( UntitledFileDoc oldPath, SavedToFile newPath ) ->
                    ( { model
                        | fileState =
                            if newPath /= oldPath then
                                FileDoc newPath

                            else
                                UntitledFileDoc oldPath
                        , docModel = { docModel | dirty = False }
                      }
                    , Cmd.none
                    )

                ( FileDoc oldPath, SavedToFile newPath ) ->
                    ( { model | fileState = FileDoc newPath, docModel = { docModel | dirty = False } }, Cmd.none )

                _ ->
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
localSaveDo mcTuple =
    sendSaveMsg SaveToFile mcTuple


sendSaveMsg : (String -> String -> Outgoing.Msg) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendSaveMsg msg ( { fileState } as model, prevCmd ) =
    let
        vstate =
            model.docModel.viewState

        treeToSave =
            case vstate.viewMode of
                Normal ->
                    model.docModel.workingTree.tree

                _ ->
                    TreeStructure.update (Upd vstate.active model.docModel.field) model.docModel.workingTree
                        |> .tree
    in
    ( model
    , Cmd.batch
        [ send <| msg (fileStateToPath fileState) (treeToMarkdownOutline False treeToSave)
        , prevCmd
        ]
    )


addToHistoryDo : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryDo ( { docModel, fileState } as model, prevCmd ) =
    let
        author =
            "<local-file>"

        metadata =
            fileStateToPath fileState
                |> Enc.string

        commitReq_ =
            Data.requestCommit docModel.workingTree.tree author docModel.data metadata
    in
    case commitReq_ of
        Just commitReq ->
            ( model
            , Cmd.batch
                [ send <| CommitData commitReq
                , prevCmd
                ]
            )

        Nothing ->
            ( model, prevCmd )



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ div [ id "app-root", applyTheme model.theme ]
        ([ viewFileSaveIndicator model.docModel.dirty ]
            ++ Page.Doc.view
                { docMsg = GotDocMsg
                , keyboard = \s -> Incoming (Keyboard s)
                , tooltipRequested = TooltipRequested
                , tooltipClosed = TooltipClosed
                }
                model.docModel
        )
    ]


viewFileSaveIndicator : Bool -> Html msg
viewFileSaveIndicator isDirty =
    div [ id "file-save-indicator" ]
        [ text <|
            if isDirty then
                "Saving..."

            else
                "Saved"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Incoming.subscribe Incoming LogErr
        ]
