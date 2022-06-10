module Electron.Electron exposing (..)

import Browser
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownOutline)
import Doc.Data as Data
import Doc.Fullscreen exposing (viewFullscreenButtonsDesktop)
import Doc.TreeStructure as TreeStructure exposing (Msg(..))
import Doc.TreeUtils exposing (getTree)
import Doc.UI as UI
import GlobalData
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (classList, id, title)
import Html.Lazy exposing (lazy5)
import Json.Decode as Dec exposing (Decoder, Value)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..), ParentMsg(..), activate, checkoutCommit, saveAndStopEditing)
import Page.Doc.Export as Export exposing (ExportFormat(..), ExportSelection(..), exportView, toExtension)
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (Theme(..), applyTheme)
import Task
import Time
import Translation exposing (Language, TranslationId, timeDistInWords)
import Types exposing (Children(..), TooltipPosition, Tree, ViewMode(..))


main : Program DataIn Model Msg
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
    , lastSave : Time.Posix
    , uiState : UIState
    , tooltip : Maybe ( Element, TooltipPosition, TranslationId )
    , theme : Theme
    }


type FileState
    = UntitledFileDoc String
    | FileDoc String


type UIState
    = DocUI
    | VersionHistoryView { start : String, currentView : String }
    | ExportPreview ( ExportSelection, ExportFormat )


fileStateToPath : FileState -> String
fileStateToPath fState =
    case fState of
        UntitledFileDoc str ->
            str

        FileDoc str ->
            str


type alias DataIn =
    { filePath : String
    , fileData : Maybe String
    , fileSettings : Value
    , undoData : Value
    , globalData : Value
    , isUntitled : Bool
    }


init : DataIn -> ( Model, Cmd Msg )
init dataIn =
    let
        globalData =
            GlobalData.decode dataIn.globalData

        lastActives =
            case Dec.decodeValue (Dec.field "last-actives" (Dec.list Dec.string)) dataIn.fileSettings of
                Ok xs ->
                    xs

                Err _ ->
                    []

        undoData =
            Data.success dataIn.undoData Data.empty

        ( initDocModel, initFileState, maybeFocus ) =
            case dataIn.fileData of
                Nothing ->
                    ( Page.Doc.init True globalData
                    , UntitledFileDoc dataIn.filePath
                    , Task.attempt (always NoOp) (Browser.Dom.focus "card-edit-1")
                    )

                Just fileData ->
                    case Coders.normalizeAndParse fileData of
                        Ok parsedTrees ->
                            ( Page.Doc.init False globalData |> initDoc (Tree "0" "" (Children parsedTrees))
                            , if dataIn.isUntitled then
                                UntitledFileDoc dataIn.filePath

                              else
                                FileDoc dataIn.filePath
                            , Cmd.none
                            )

                        Err err ->
                            ( Page.Doc.init True globalData
                            , UntitledFileDoc "parser error"
                            , Task.attempt (always NoOp) (Browser.Dom.focus "card-edit-1")
                            )

        maybeLocalSave ( m, c ) =
            if dataIn.isUntitled && dataIn.fileData /= Nothing then
                localSaveDo ( m, c )

            else
                ( m, c )

        ( updateDocModel, docCmd ) =
            let
                vs =
                    initDocModel.viewState

                ( newViewState, maybeActivate ) =
                    case lastActives of
                        fst :: rest ->
                            ( { vs | active = fst, activePast = rest }
                            , activate fst True
                            )

                        _ ->
                            ( vs, activate "1" True )
            in
            ( { initDocModel
                | data = undoData
                , viewState = newViewState
              }
            , Cmd.none
            )
                |> maybeActivate
    in
    ( { docModel = updateDocModel
      , fileState = initFileState
      , lastSave = GlobalData.currentTime globalData
      , uiState = DocUI
      , tooltip = Nothing
      , theme = Default
      }
    , Cmd.batch [ maybeFocus, Cmd.map GotDocMsg docCmd ]
    )
        |> maybeLocalSave


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
      --
    | HistoryToggled Bool
    | CheckoutCommit String
    | Restore
      --
    | CloseExport
    | ExportFormatChanged ExportFormat
    | ExportSelectionChanged ExportSelection
    | Export
      --
    | TooltipRequested String TooltipPosition TranslationId
    | TooltipReceived Element TooltipPosition TranslationId
    | TooltipClosed
      --
    | ExitFullscreenRequested
    | SaveAndExitFullscreen
      --
    | TimeUpdate Time.Posix
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

        HistoryToggled isOpen ->
            if isOpen then
                ( { model | uiState = VersionHistoryView { start = "ffadsf", currentView = "salkfjsda" } }, Cmd.none )

            else
                ( { model | uiState = DocUI }, Cmd.none )

        CheckoutCommit commitSha ->
            case model.uiState of
                VersionHistoryView historyState ->
                    let
                        ( newDocModel, docCmd ) =
                            checkoutCommit commitSha docModel
                    in
                    ( { model
                        | docModel = newDocModel
                        , uiState = VersionHistoryView { historyState | currentView = commitSha }
                      }
                    , Cmd.map GotDocMsg docCmd
                    )

                _ ->
                    ( model, Cmd.none )

        Restore ->
            ( { model | uiState = DocUI }
            , Cmd.none
            )
                |> localSaveDo

        --
        CloseExport ->
            case model.uiState of
                ExportPreview _ ->
                    ( { model | uiState = DocUI }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExportFormatChanged newExpFormat ->
            case model.uiState of
                ExportPreview ( oldExpSelection, oldExpFormat ) ->
                    ( { model | uiState = ExportPreview ( oldExpSelection, newExpFormat ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ExportSelectionChanged newExpSelection ->
            case model.uiState of
                ExportPreview ( oldExpSelection, oldExpFormat ) ->
                    ( { model | uiState = ExportPreview ( newExpSelection, oldExpFormat ) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Export ->
            case ( model.uiState, getTree model.docModel.viewState.active model.docModel.workingTree.tree ) of
                ( ExportPreview ( expSel, expFormat ), Just activeTree ) ->
                    ( model
                    , send <|
                        ExportToFile (toExtension expFormat)
                            (Export.toString (fileStateToPath model.fileState)
                                ( expSel, expFormat )
                                activeTree
                                model.docModel.workingTree.tree
                            )
                    )

                _ ->
                    ( model, Cmd.none )

        TimeUpdate newTime ->
            let
                newGlobalData =
                    model.docModel.globalData
                        |> GlobalData.updateTime newTime
            in
            ( { model | docModel = { docModel | globalData = newGlobalData } }, Cmd.none )

        Incoming incomingMsg ->
            let
                passthrough =
                    Page.Doc.incoming incomingMsg docModel
                        |> Tuple.mapBoth (\m -> { model | docModel = m }) (Cmd.map GotDocMsg)
            in
            case incomingMsg of
                SavedToFile newPath savedTime ->
                    let
                        oldPath =
                            fileStateToPath model.fileState

                        newGlobalData =
                            model.docModel.globalData |> GlobalData.updateTime savedTime
                    in
                    if newPath /= oldPath then
                        ( { model | fileState = FileDoc newPath, docModel = { docModel | dirty = False, globalData = newGlobalData }, lastSave = savedTime }, Cmd.none )

                    else
                        ( { model | docModel = { docModel | dirty = False, globalData = newGlobalData }, lastSave = savedTime }, Cmd.none )

                ClickedExport ->
                    ( { model | uiState = ExportPreview ( ExportEverything, DOCX ) }, Cmd.none )

                Keyboard "mod+z" ->
                    if model.docModel.viewState.viewMode == Normal then
                        openHistorySlider model

                    else
                        passthrough

                _ ->
                    passthrough

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

        --
        ExitFullscreenRequested ->
            -- TODO:
            ( model, Cmd.none )

        SaveAndExitFullscreen ->
            let
                ( newDocModel, newDocCmd ) =
                    docModel
                        |> saveAndStopEditing
            in
            ( { model | docModel = newDocModel }, Cmd.map GotDocMsg newDocCmd )

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


openHistorySlider : Model -> ( Model, Cmd Msg )
openHistorySlider model =
    case Data.head "heads/master" model.docModel.data of
        Just refObj ->
            ( { model | uiState = VersionHistoryView { start = refObj.value, currentView = refObj.value } }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )



-- VIEW


view : Model -> List (Html Msg)
view ({ docModel } as model) =
    let
        globalData =
            model.docModel.globalData

        lang =
            GlobalData.language globalData

        activeTree_ =
            getTree model.docModel.viewState.active model.docModel.workingTree.tree

        isFullscreen =
            case docModel.viewState.viewMode of
                FullscreenEditing ->
                    True

                _ ->
                    False

        exportViewOk expSettings =
            lazy5 exportView
                { export = Export
                , printRequested = NoOp
                , tooltipRequested = TooltipRequested
                , tooltipClosed = TooltipClosed
                }
                (fileStateToPath model.fileState)
                expSettings

        maybeExportView expSettings =
            case activeTree_ of
                Just activeTree ->
                    exportViewOk expSettings activeTree model.docModel.workingTree.tree

                Nothing ->
                    text ""

        viewTooltip =
            case model.tooltip of
                Just tooltip ->
                    UI.viewTooltip lang tooltip

                Nothing ->
                    text ""
    in
    [ div [ id "desktop-root", applyTheme model.theme ]
        ([ viewFileSaveIndicator
            { language = lang
            , dirty = model.docModel.dirty
            , isFullscreen = isFullscreen
            , lastSave = model.lastSave
            , currentTime = GlobalData.currentTime globalData
            }
         ]
            ++ Page.Doc.view
                { docMsg = GotDocMsg
                , keyboard = \s -> Incoming (Keyboard s)
                , tooltipRequested = TooltipRequested
                , tooltipClosed = TooltipClosed
                }
                model.docModel
            ++ (case model.uiState of
                    DocUI ->
                        []

                    VersionHistoryView histViewData ->
                        [ UI.viewHistory lang
                            { cancel = HistoryToggled False
                            , checkout = CheckoutCommit
                            , restore = Restore
                            , noOp = NoOp
                            , tooltipClosed = TooltipClosed
                            , tooltipRequested = TooltipRequested
                            }
                            (GlobalData.currentTime globalData)
                            model.docModel.data
                            histViewData
                        ]

                    ExportPreview exportSettings ->
                        [ UI.viewExportMenu lang
                            { exportFormatChanged = ExportFormatChanged
                            , exportSelectionChanged = ExportSelectionChanged
                            , tooltipRequested = TooltipRequested
                            , tooltipClosed = TooltipClosed
                            , toggledExport = CloseExport
                            }
                            True
                            exportSettings
                        , maybeExportView exportSettings
                        ]
               )
            ++ (if isFullscreen then
                    viewFullscreenButtonsDesktop
                        { exitFullscreenRequested = ExitFullscreenRequested
                        , saveAndExitFullscreen = SaveAndExitFullscreen
                        }
                        { isMac = GlobalData.isMac docModel.globalData
                        , dirty = docModel.dirty
                        }
                        |> List.singleton

                else
                    []
               )
            ++ [ viewTooltip ]
        )
    ]


viewFileSaveIndicator : { language : Language, dirty : Bool, isFullscreen : Bool, lastSave : Time.Posix, currentTime : Time.Posix } -> Html msg
viewFileSaveIndicator { language, dirty, isFullscreen, lastSave, currentTime } =
    let
        lastSaveInWords =
            if abs (Time.posixToMillis lastSave - Time.posixToMillis currentTime) < 3000 then
                "Just now"

            else
                timeDistInWords language lastSave currentTime
    in
    div
        [ id "file-save-indicator"
        , classList [ ( "dirty", dirty ), ( "fullscreen", isFullscreen ) ]
        , title lastSaveInWords
        ]
        [ text <|
            if dirty then
                "Unsaved changes..."

            else
                "All Changes Saved"
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Incoming.subscribe Incoming LogErr
        , Time.every (9 * 1000) TimeUpdate
        ]
