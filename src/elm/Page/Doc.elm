module Page.Doc exposing (Model, Msg, getTitle, init, subscriptions, toUser, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Bytes exposing (Bytes)
import Coders exposing (sortByEncoder, treeToMarkdownString, treeToValue)
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, toDebouncer)
import Doc.ContactForm as ContactForm
import Doc.Data as Data
import Doc.Data.Conflict exposing (Selection)
import Doc.Fonts as Fonts
import Doc.Fullscreen as Fullscreen
import Doc.HelpScreen as HelpScreen
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.Switcher
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils exposing (..)
import Doc.UI as UI exposing (countWords, viewConflict, viewMobileButtons, viewSearchField)
import Doc.VideoViewer as VideoViewer
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Html exposing (Attribute, Html, div, h1, span, text, textarea, ul)
import Html.Attributes as Attributes exposing (attribute, class, classList, dir, id, style, title, value)
import Html.Events exposing (custom, onClick, onDoubleClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3, lazy4, lazy7, lazy8)
import Html5.DragDrop as DragDrop
import Http
import Import.Bulk.UI as ImportModal
import Import.Incoming
import Import.Markdown
import Import.Opml
import Import.Single
import Json.Decode as Json
import Json.Encode as Enc
import List.Extra as ListExtra
import Markdown
import Outgoing exposing (Msg(..), send)
import Page.Doc.Export as Export exposing (ExportFormat(..), ExportSelection(..), exportView, exportViewError)
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme as Theme exposing (Theme(..), applyTheme)
import Process
import Random
import RandomId
import Regex
import Route
import Session exposing (Session)
import SharedUI exposing (ctrlOrCmdText)
import Svg exposing (circle, mask, rect, svg)
import Svg.Attributes exposing (cx, cy, height, preserveAspectRatio, r, rx, ry, viewBox, width, x, y)
import Task
import Time
import Translation exposing (Language, TranslationId(..), langToString, tr)
import Types exposing (..)
import Upgrade exposing (Msg(..))
import Utils exposing (randomPositiveInt)



-- MODEL


type alias Model =
    -- Document state
    { workingTree : TreeStructure.Model
    , data : Data.Model
    , metadata : Metadata

    -- SPA Page State
    , session : Session
    , loading : Bool

    -- Transient state
    , viewState : ViewState
    , dirty : Bool
    , lastLocalSave : Maybe Time.Posix
    , lastRemoteSave : Maybe Time.Posix
    , field : String
    , textCursorInfo : TextCursorInfo
    , debouncerStateCommit : Debouncer () ()
    , titleField : Maybe String
    , sidebarState : SidebarState
    , sidebarMenuState : SidebarMenuState
    , modalState : ModalState
    , fileSearchField : String
    , headerMenu : HeaderMenuState
    , exportSettings : ( ExportSelection, ExportFormat )
    , wordcountTrayOpen : Bool
    , tooltip : Maybe ( Element, TooltipPosition, String )
    , fontSelectorOpen : Bool

    -- Settings
    , uid : String
    , fonts : Fonts.Model
    , theme : Theme
    , startingWordcount : Int
    }


type ModalState
    = NoModal
    | FileSwitcher Doc.Switcher.Model
    | SidebarContextMenu String ( Float, Float )
    | TemplateSelector
    | HelpScreen
    | VideoViewer VideoViewer.Model
    | Wordcount
    | ImportModal ImportModal.Model
    | ContactForm ContactForm.Model
    | UpgradeModal


defaultModel : Bool -> Session -> String -> Model
defaultModel isNew session docId =
    { workingTree = TreeStructure.defaultModel
    , data = Data.empty
    , metadata = Metadata.new docId
    , session = session
    , loading = not isNew
    , debouncerStateCommit =
        Debouncer.throttle (fromSeconds 3)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 3)
            |> toDebouncer
    , uid = "0"
    , viewState =
        { active = "1"
        , viewMode =
            if isNew then
                Editing

            else
                Normal
        , activePast = []
        , descendants = []
        , ancestors = [ "0" ]
        , searchField = Nothing
        , dragModel = ( DragDrop.init, DragExternalModel Nothing False )
        , draggedTree = Nothing
        , copiedTree = Nothing
        , clipboardTree = Nothing
        , collaborators = []
        }
    , dirty = False
    , lastLocalSave = Nothing
    , lastRemoteSave = Nothing
    , field = ""
    , textCursorInfo = { selected = False, position = End, text = ( "", "" ) }
    , titleField = Nothing
    , sidebarState =
        if Session.fileMenuOpen session then
            File

        else
            SidebarClosed
    , sidebarMenuState = NoSidebarMenu
    , modalState = NoModal
    , fileSearchField = ""
    , headerMenu = NoHeaderMenu
    , exportSettings = ( ExportEverything, DOCX )
    , wordcountTrayOpen = False
    , tooltip = Nothing
    , fontSelectorOpen = False
    , fonts = Fonts.default
    , theme = Default
    , startingWordcount = 0
    }


init : Session -> String -> Bool -> ( Model, Cmd Msg )
init session dbName isNew =
    let
        initModel =
            defaultModel isNew session dbName
    in
    if isNew then
        ( initModel
        , Cmd.batch
            [ send <| InitDocument dbName
            , focus "1"
            ]
        )
            |> activate "1" True
            |> addToHistoryDo

    else
        ( initModel, send <| LoadDocument dbName )


toUser : Model -> Session
toUser model =
    model.session


getTitle : Model -> String
getTitle model =
    Metadata.getDocName model.metadata
        |> Maybe.withDefault "Untitled"



-- UPDATE


type Msg
    = NoOp
      -- === Card Activation ===
    | Activate String
    | SearchFieldUpdated String
      -- === Card Editing  ===
    | OpenCard String String
    | UpdateActiveField String String
    | AutoSave
    | SaveAndCloseCard
    | EditToFullscreenMode
    | FullscreenMsg Fullscreen.Msg
    | DeleteCard String
      -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
      -- === Dragging ===
    | DragDropMsg (DragDrop.Msg String DropId)
    | DragExternal DragExternalMsg
      -- === History ===
    | ThrottledCommit (Debouncer.Msg ())
    | Commit Time.Posix
    | CheckoutCommit String
    | Restore
    | CancelHistory
    | SetSelection String Selection String
    | Resolve String
      -- === UI ===
    | ReceivedDocuments DocList.Model
    | SettingsChanged Json.Value
    | LogoutRequested
    | LoginStateChanged Session
    | TitleFocused
    | TitleFieldChanged String
    | TitleEdited
    | TitleEditCanceled
    | ToggledHelpMenu Bool
    | LanguageMenuRequested (Maybe String)
    | LanguageMenuReceived Element
    | ToggledAccountMenu Bool
      -- Sidebar & Modals
    | SidebarStateChanged SidebarState
    | TemplateSelectorOpened
    | VideoViewerOpened
    | VideoViewerMsg VideoViewer.Msg
    | SwitcherOpened
    | SwitcherClosed
    | WordcountModalOpened
    | ModalClosed
    | ImportBulkClicked
    | ImportMarkdownRequested
    | ImportOpmlRequested
    | ImportJSONRequested
    | FileSearchChanged String
    | SortByChanged SortBy
    | SidebarContextClicked String ( Float, Float )
    | DuplicateDoc String
    | DeleteDoc String
    | HistoryToggled Bool
    | DocSettingsToggled Bool
    | ExportPreviewToggled Bool
    | ExportSelectionChanged ExportSelection
    | ExportFormatChanged ExportFormat
      -- Upgrade
    | ToggledUpgradeModal Bool
    | UpgradeModalMsg Upgrade.Msg
      -- HELP
    | ClickedShowVideos
    | ClickedShowWidget
    | ClickedEmailSupport
    | ContactFormMsg ContactForm.Model ContactForm.Msg
    | CopyEmailClicked Bool
    | ContactFormSubmitted ContactForm.Model
    | ContactFormSent (Result Http.Error ())
      -- Import
    | ImportModalMsg ImportModal.Msg
    | ImportMarkdownSelected File (List File)
    | ImportMarkdownLoaded (List String) (List String)
    | ImportMarkdownIdGenerated Tree String
    | ImportOpmlSelected File
    | ImportOpmlLoaded String String
    | ImportOpmlIdGenerated Tree String String
    | ImportOpmlCompleted String
    | ImportJSONSelected File
    | ImportJSONLoaded String String
    | ImportJSONIdGenerated Tree String String
    | ImportJSONCompleted String
    | ImportBulkCompleted
      -- Misc UI
    | LanguageChanged Language
    | ThemeChanged Theme
    | TooltipRequested String TooltipPosition String
    | TooltipReceived Element TooltipPosition String
    | TooltipClosed
    | FullscreenRequested
    | PrintRequested
    | TimeUpdate Time.Posix
    | FontsMsg Fonts.Msg
    | ShortcutTrayToggle
      -- === Ports ===
    | Pull
    | Export
    | Exported String (Result Http.Error Bytes)
    | Incoming Incoming.Msg
    | LogErr String


type DragExternalMsg
    = DragEnter DropId
    | DragLeave DropId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ workingTree } as model) =
    let
        vs =
            model.viewState
    in
    case msg of
        -- === Card Activation ===
        Activate id ->
            ( model
            , Cmd.none
            )
                |> saveCardIfEditing
                |> activate id False

        SearchFieldUpdated inputField ->
            let
                searchFilter term_ cols =
                    case term_ of
                        Just term ->
                            let
                                hasTerm tree =
                                    term
                                        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
                                        |> Maybe.withDefault Regex.never
                                        |> (\t -> Regex.contains t tree.content)
                            in
                            cols
                                |> List.map (\c -> List.map (\g -> List.filter hasTerm g) c)

                        Nothing ->
                            cols

                ( maybeBlur, newSearchField ) =
                    case inputField of
                        "" ->
                            ( \( m, c ) ->
                                ( m
                                , Cmd.batch [ c, Task.attempt (\_ -> NoOp) (Browser.Dom.blur "search-input") ]
                                )
                            , Nothing
                            )

                        str ->
                            ( identity
                            , Just str
                            )

                filteredCardIds =
                    searchFilter newSearchField model.workingTree.columns
                        |> List.map (\c -> List.map (\g -> List.map .id g) c)
                        |> List.concat
                        |> List.concat

                allCardsInOrder =
                    getDescendants model.workingTree.tree
                        |> List.map .id

                firstFilteredCardId_ =
                    ListExtra.find (\cId -> List.member cId filteredCardIds) allCardsInOrder

                maybeActivate =
                    case ( newSearchField, firstFilteredCardId_ ) of
                        ( Just _, Just id ) ->
                            activate id False

                        ( Nothing, _ ) ->
                            activate vs.active False

                        _ ->
                            identity
            in
            ( { model | viewState = { vs | searchField = newSearchField } }
            , Cmd.none
            )
                |> maybeBlur
                |> maybeActivate

        -- === Card Editing  ===
        OpenCard id str ->
            ( model
            , Cmd.none
            )
                |> openCard id str

        UpdateActiveField id str ->
            ( { model
                | field = str
                , dirty = True
              }
            , Cmd.batch
                [ send <| SetDirty True
                , send <| SetTextareaClone id str
                ]
            )

        AutoSave ->
            ( model, Cmd.none ) |> saveCardIfEditing

        SaveAndCloseCard ->
            saveAndStopEditing model

        EditToFullscreenMode ->
            model |> enterFullscreen

        FullscreenMsg fullscreenMsg ->
            case fullscreenMsg of
                Fullscreen.OpenCard id str ->
                    ( model
                    , Cmd.none
                    )
                        |> saveCardIfEditing
                        |> openCardFullscreen id str

                Fullscreen.UpdateField id str ->
                    ( { model | dirty = True, field = str }
                    , send <| SetDirty True
                    )
                        |> (if vs.active /= id then
                                activate id False

                            else
                                identity
                           )

                Fullscreen.SaveChanges ->
                    ( model, Cmd.none )
                        |> saveCardIfEditing

                Fullscreen.ExitFullscreenRequested ->
                    exitFullscreen model

        DeleteCard id ->
            ( model
            , Cmd.none
            )
                |> deleteCard id

        -- === Card Insertion  ===
        InsertAbove id ->
            ( model
            , Cmd.none
            )
                |> insertAbove id ""

        InsertBelow id ->
            ( model
            , Cmd.none
            )
                |> insertBelow id ""

        InsertChild id ->
            ( model
            , Cmd.none
            )
                |> insertChild id ""

        -- === Card Moving  ===
        DragDropMsg dragDropMsg ->
            let
                ( newDragModel, dragResult_ ) =
                    DragDrop.update dragDropMsg (Tuple.first vs.dragModel)

                modelDragUpdated =
                    { model
                        | viewState =
                            { vs
                                | dragModel = ( newDragModel, Tuple.second vs.dragModel )
                            }
                    }
            in
            case ( DragDrop.getDragId newDragModel, dragResult_ ) of
                ( Just _, Nothing ) ->
                    -- Dragging
                    ( modelDragUpdated
                    , DragDrop.getDragstartEvent dragDropMsg
                        |> Maybe.map .event
                        |> Maybe.map (\json -> send <| DragStart json)
                        |> Maybe.withDefault Cmd.none
                    )

                ( Nothing, Just ( _, dropId, _ ) ) ->
                    -- Drop success
                    case vs.draggedTree of
                        Just ( draggedTree, _, _ ) ->
                            let
                                moveOperation =
                                    case dropId of
                                        Into id ->
                                            move draggedTree id 999999

                                        Above id ->
                                            move draggedTree
                                                ((getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0")
                                                ((getIndex id model.workingTree.tree |> Maybe.withDefault 0) |> Basics.max 0)

                                        Below id ->
                                            move draggedTree
                                                ((getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0")
                                                ((getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1)
                            in
                            ( { modelDragUpdated | viewState = { vs | draggedTree = Nothing }, dirty = True }, Cmd.batch [ send <| SetDirty True, send <| DragDone ] )
                                |> moveOperation

                        Nothing ->
                            ( modelDragUpdated, Cmd.none )

                ( Nothing, Nothing ) ->
                    -- NotDragging
                    case vs.draggedTree of
                        Just ( draggedTree, parentId, idx ) ->
                            ( modelDragUpdated, Cmd.none )
                                |> move draggedTree parentId idx

                        Nothing ->
                            ( modelDragUpdated, Cmd.none )

                ( Just _, Just _ ) ->
                    -- Should be Impossible: both Dragging and Dropped
                    ( modelDragUpdated, Cmd.none )

        DragExternal dragExternalMsg ->
            case dragExternalMsg of
                DragEnter dId ->
                    ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Just dId, isDragging = True } ) } }, Cmd.none )

                DragLeave dId ->
                    if (Tuple.second vs.dragModel |> .dropId) == Just dId then
                        ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = True } ) } }, Cmd.none )

                    else
                        ( model, Cmd.none )

        -- === History ===
        ThrottledCommit subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerStateCommit

                mappedCmd =
                    Cmd.map ThrottledCommit subCmd

                updatedModel =
                    { model | debouncerStateCommit = subModel }
            in
            case emitted_ of
                Just () ->
                    ( updatedModel
                    , Cmd.batch [ Task.perform Commit Time.now, mappedCmd ]
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        Commit time ->
            ( { model | session = Session.updateTime time model.session }, Cmd.none )
                |> addToHistoryDo

        CheckoutCommit commitSha ->
            case model.headerMenu of
                HistoryView historyState ->
                    ( { model | headerMenu = HistoryView { historyState | currentView = commitSha } }
                    , Cmd.none
                    )
                        |> checkoutCommit commitSha

                _ ->
                    ( model, Cmd.none )

        Restore ->
            ( { model | headerMenu = NoHeaderMenu }
            , Cmd.none
            )
                |> addToHistoryDo

        CancelHistory ->
            case model.headerMenu of
                HistoryView historyState ->
                    ( { model | headerMenu = NoHeaderMenu, tooltip = Nothing }
                    , Cmd.none
                    )
                        |> checkoutCommit historyState.start

                _ ->
                    ( model
                    , Cmd.none
                    )

        SetSelection cid selection id ->
            let
                newData =
                    Data.conflictSelection cid selection model.data
            in
            ( { model
                | data = newData
                , workingTree = TreeStructure.setTreeWithConflicts (Data.conflictList newData) model.workingTree.tree model.workingTree
              }
            , Cmd.none
            )
                |> activate id True

        Resolve cid ->
            let
                newData =
                    Data.resolve cid model.data
            in
            ( { model
                | data = newData
                , workingTree = TreeStructure.setTreeWithConflicts (Data.conflictList newData) model.workingTree.tree model.workingTree
              }
            , Cmd.none
            )
                |> addToHistory

        -- === UI ===
        ReceivedDocuments newListState ->
            let
                updatedSession =
                    Session.updateDocuments newListState model.session

                ( newModel, newCmd ) =
                    case DocList.current model.metadata newListState of
                        Just currentMetadata ->
                            ( { model | metadata = currentMetadata, titleField = Metadata.getDocName currentMetadata, session = updatedSession }, Cmd.none )

                        Nothing ->
                            ( { model | session = updatedSession }
                            , Route.replaceUrl (Session.navKey model.session) Route.Root
                            )
            in
            ( newModel, newCmd )

        SettingsChanged json ->
            ( { model | session = Session.sync json model.session }, Cmd.none )

        TitleFocused ->
            case model.titleField of
                Nothing ->
                    ( model, send <| SelectAll "title-rename" )

                Just _ ->
                    ( model, Cmd.none )

        TitleFieldChanged newTitle ->
            ( { model | titleField = Just newTitle }, Cmd.none )

        TitleEdited ->
            case model.titleField of
                Just editedTitle ->
                    if String.trim editedTitle == "" then
                        ( model, Cmd.batch [ send <| Alert "Title cannot be blank", Task.attempt (always NoOp) (Browser.Dom.focus "title-rename") ] )

                    else if Just editedTitle /= Metadata.getDocName model.metadata then
                        ( model, Cmd.batch [ send <| RenameDocument editedTitle, Task.attempt (always NoOp) (Browser.Dom.blur "title-rename") ] )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        TitleEditCanceled ->
            ( { model | titleField = Metadata.getDocName model.metadata }, Task.attempt (always NoOp) (Browser.Dom.blur "title-rename") )

        LogoutRequested ->
            ( model, Session.logout )

        LoginStateChanged newUser ->
            ( { model | session = newUser }, Route.pushUrl (Session.navKey newUser) Route.Login )

        ToggledHelpMenu isOpen ->
            ( { model | modalState = HelpScreen }, Cmd.none )

        LanguageMenuRequested elId_ ->
            case ( elId_, model.sidebarMenuState ) of
                ( Just elId, Account _ ) ->
                    ( model
                    , Browser.Dom.getElement elId
                        |> Task.attempt
                            (\result ->
                                case result of
                                    Ok el ->
                                        LanguageMenuReceived el

                                    Err _ ->
                                        NoOp
                            )
                    )

                _ ->
                    ( { model | sidebarMenuState = Account Nothing }, Cmd.none )

        LanguageMenuReceived el ->
            ( { model | sidebarMenuState = Account (Just el) }, Cmd.none )

        ToggledAccountMenu isOpen ->
            let
                ( newDropdownState, newSidebarState ) =
                    if isOpen then
                        ( Account Nothing, SidebarClosed )

                    else
                        ( NoSidebarMenu, model.sidebarState )
            in
            ( { model
                | sidebarMenuState = newDropdownState
                , sidebarState = newSidebarState
                , tooltip = Nothing
              }
            , Cmd.none
            )

        ToggledUpgradeModal isOpen ->
            ( { model
                | modalState =
                    if isOpen then
                        UpgradeModal

                    else
                        NoModal
              }
            , Cmd.none
            )

        UpgradeModalMsg upgradeModalMsg ->
            case upgradeModalMsg of
                UpgradeModalClosed ->
                    ( { model | modalState = NoModal }, Cmd.none )

                CheckoutClicked checkoutData ->
                    case Session.name model.session of
                        Just email ->
                            let
                                data =
                                    Upgrade.toValue email checkoutData
                            in
                            ( model, send <| CheckoutButtonClicked data )

                        Nothing ->
                            ( model, Cmd.none )

                _ ->
                    let
                        newSession =
                            Session.updateUpgrade upgradeModalMsg model.session

                        maybeFlash =
                            case upgradeModalMsg of
                                PlanChanged _ ->
                                    send <| FlashPrice

                                _ ->
                                    Cmd.none
                    in
                    ( { model | session = newSession }, maybeFlash )

        ClickedShowVideos ->
            ( { model | modalState = VideoViewer VideoViewer.init, sidebarMenuState = NoSidebarMenu }, Cmd.none )

        ClickedShowWidget ->
            ( { model | modalState = NoModal }, send <| ShowWidget )

        ClickedEmailSupport ->
            let
                fromEmail =
                    Session.name model.session
                        |> Maybe.withDefault ""
            in
            ( { model | modalState = ContactForm (ContactForm.init fromEmail), sidebarMenuState = NoSidebarMenu }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "contact-body")
            )

        ContactFormMsg formModel formMsg ->
            ( { model | modalState = ContactForm (ContactForm.update formMsg formModel) }, Cmd.none )

        CopyEmailClicked isUrgent ->
            if isUrgent then
                ( model, send <| CopyToClipboard "{%SUPPORT_URGENT_EMAIL%}" "#email-copy-btn" )

            else
                ( model, send <| CopyToClipboard "{%SUPPORT_EMAIL%}" "#email-copy-btn" )

        ContactFormSubmitted formModel ->
            ( model, ContactForm.send ContactFormSent formModel )

        ContactFormSent res ->
            case res of
                Ok _ ->
                    ( { model | modalState = NoModal }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SidebarStateChanged newSidebarState ->
            let
                ( newSessionData, maybeSaveSidebarState ) =
                    case newSidebarState of
                        File ->
                            ( Session.setFileOpen True model.session, send <| SetSidebarState True )

                        _ ->
                            ( Session.setFileOpen True model.session, send <| SetSidebarState False )

                newDropdownState =
                    case ( newSidebarState, model.sidebarMenuState ) of
                        ( File, Help ) ->
                            NoSidebarMenu

                        ( File, Account _ ) ->
                            NoSidebarMenu

                        ( _, _ ) ->
                            model.sidebarMenuState
            in
            ( { model
                | session = newSessionData
                , sidebarState = newSidebarState
                , tooltip = Nothing
                , sidebarMenuState = newDropdownState
              }
            , maybeSaveSidebarState
            )

        TemplateSelectorOpened ->
            ( { model | modalState = TemplateSelector }, Cmd.none )

        VideoViewerOpened ->
            ( { model | modalState = VideoViewer VideoViewer.init }, Cmd.none )

        VideoViewerMsg videoViewerMsg ->
            ( { model | modalState = VideoViewer (VideoViewer.update videoViewerMsg) }, Cmd.none )

        SwitcherOpened ->
            openSwitcher model

        SwitcherClosed ->
            closeSwitcher model

        WordcountModalOpened ->
            ( { model | modalState = Wordcount, headerMenu = NoHeaderMenu }, Cmd.none )

        ModalClosed ->
            case model.modalState of
                VideoViewer _ ->
                    ( { model | modalState = HelpScreen }, Cmd.none )

                ContactForm _ ->
                    ( { model | modalState = HelpScreen }, Cmd.none )

                _ ->
                    ( { model | modalState = NoModal }, Cmd.none )

        ImportBulkClicked ->
            ( { model | modalState = ImportModal (ImportModal.init model.session) }, Cmd.none )

        FileSearchChanged term ->
            let
                updatedModal =
                    case model.modalState of
                        FileSwitcher switcherModel ->
                            FileSwitcher (Doc.Switcher.search term switcherModel)

                        _ ->
                            model.modalState
            in
            ( { model | fileSearchField = term, modalState = updatedModal }, Cmd.none )

        SortByChanged newSort ->
            let
                newSession =
                    Session.setSortBy newSort model.session
            in
            ( { model | session = newSession }, send <| SaveUserSetting ( "sortBy", sortByEncoder newSort ) )

        SidebarContextClicked docId ( x, y ) ->
            ( { model | modalState = SidebarContextMenu docId ( x, y ) }, Cmd.none )

        DuplicateDoc docId ->
            ( { model | modalState = NoModal }, Route.replaceUrl (Session.navKey model.session) (Route.Copy docId) )

        DeleteDoc docId ->
            ( { model | modalState = NoModal }, send <| RequestDelete docId )

        HistoryToggled isOpen ->
            model |> toggleHistory isOpen

        DocSettingsToggled isOpen ->
            ( { model
                | headerMenu =
                    if isOpen then
                        Settings

                    else
                        NoHeaderMenu
                , tooltip = Nothing
              }
            , Cmd.none
            )

        ExportPreviewToggled previewEnabled ->
            ( { model
                | headerMenu =
                    if previewEnabled then
                        ExportPreview

                    else
                        NoHeaderMenu
                , tooltip = Nothing
              }
            , Cmd.none
            )
                |> activate vs.active True

        ExportSelectionChanged expSel ->
            ( { model | exportSettings = Tuple.mapFirst (always expSel) model.exportSettings }, Cmd.none )

        ExportFormatChanged expFormat ->
            ( { model | exportSettings = Tuple.mapSecond (always expFormat) model.exportSettings }, Cmd.none )

        ImportModalMsg modalMsg ->
            case model.modalState of
                ImportModal importModal ->
                    let
                        ( newModalState, newCmd ) =
                            ImportModal.update modalMsg importModal
                                |> Tuple.mapBoth ImportModal (Cmd.map ImportModalMsg)
                    in
                    ( { model | modalState = newModalState }, newCmd )

                _ ->
                    ( model, Cmd.none )

        ImportMarkdownRequested ->
            ( model, Select.files [ ".md", ".markdown", ".mdown", "text/markdown", "text/x-markdown", "text/plain" ] ImportMarkdownSelected )

        ImportMarkdownSelected firstFile restFiles ->
            let
                tasks =
                    firstFile :: restFiles |> List.map File.toString |> Task.sequence

                metadata =
                    firstFile
                        :: restFiles
                        |> List.map File.name
            in
            ( model, Task.perform (ImportMarkdownLoaded metadata) tasks )

        ImportMarkdownLoaded metadata markdownStrings ->
            let
                ( importedTree, newSeed ) =
                    Import.Markdown.toTree (Session.seed model.session) metadata markdownStrings

                newSession =
                    Session.setSeed newSeed model.session
            in
            ( { model | loading = True, session = newSession }, RandomId.generate (ImportMarkdownIdGenerated importedTree) )

        ImportMarkdownIdGenerated tree docId ->
            let
                author =
                    model.session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> Metadata.encode)
            in
            case commitReq_ of
                Just commitReq ->
                    ( model, send <| SaveImportedData commitReq )

                Nothing ->
                    ( model, Cmd.none )

        ImportOpmlRequested ->
            ( model, Select.file [ "application/xml", "text/xml", "text/x-opml", ".opml" ] ImportOpmlSelected )

        ImportOpmlSelected file ->
            ( model, Task.perform (ImportOpmlLoaded (File.name file)) (File.toString file) )

        ImportOpmlLoaded fileName opmlString ->
            let
                ( importTreeResult, newSeed ) =
                    Import.Opml.treeResult (Session.seed model.session) opmlString

                newSession =
                    Session.setSeed newSeed model.session
            in
            case importTreeResult of
                Ok tree ->
                    ( { model | loading = True, session = newSession }
                    , RandomId.generate (ImportOpmlIdGenerated tree fileName)
                    )

                Err err ->
                    ( { model | session = newSession }, Cmd.none )

        ImportOpmlIdGenerated tree fileName docId ->
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

        ImportOpmlCompleted docId ->
            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

        ImportJSONRequested ->
            ( model, Select.file [ "application/json", "text/plain" ] ImportJSONSelected )

        ImportJSONSelected file ->
            ( model, Task.perform (ImportJSONLoaded (File.name file)) (File.toString file) )

        ImportJSONLoaded fileName jsonString ->
            let
                ( importTreeDecoder, newSeed ) =
                    Import.Single.decoder (Session.seed model.session)

                newSession =
                    Session.setSeed newSeed model.session
            in
            case Json.decodeString importTreeDecoder jsonString of
                Ok tree ->
                    ( { model | loading = True, session = newSession }
                    , RandomId.generate (ImportJSONIdGenerated tree fileName)
                    )

                Err err ->
                    ( { model | session = newSession }, Cmd.none )

        ImportJSONIdGenerated tree fileName docId ->
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

        ImportJSONCompleted docId ->
            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

        ImportBulkCompleted ->
            ( { model | modalState = NoModal }, Cmd.none )

        LanguageChanged newLang ->
            if newLang /= Session.language model.session then
                ( { model
                    | session = Session.setLanguage newLang model.session
                    , sidebarMenuState = NoSidebarMenu
                  }
                , send <| SaveUserSetting ( "language", langToString newLang |> Enc.string )
                )

            else
                ( model, Cmd.none )

        ThemeChanged newTheme ->
            ( { model | theme = newTheme }, send <| SaveThemeSetting newTheme )

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

        FullscreenRequested ->
            ( model, send <| RequestFullscreen )

        PrintRequested ->
            ( model, send <| Print )

        TimeUpdate time ->
            ( { model | session = Session.updateTime time model.session }
            , Cmd.none
            )

        FontsMsg fontsMsg ->
            let
                ( newModel, selectorOpen, newFontsTriple_ ) =
                    Fonts.update fontsMsg model.fonts

                cmd =
                    case newFontsTriple_ of
                        Just newFontsTriple ->
                            send (SetFonts newFontsTriple)

                        Nothing ->
                            Cmd.none
            in
            ( { model | fonts = newModel, fontSelectorOpen = selectorOpen }
            , cmd
            )

        ShortcutTrayToggle ->
            let
                newIsOpen =
                    not <| Session.shortcutTrayOpen model.session
            in
            ( { model
                | session = Session.setShortcutTrayOpen newIsOpen model.session
                , headerMenu =
                    if model.headerMenu == ExportPreview && newIsOpen then
                        NoHeaderMenu

                    else
                        model.headerMenu
                , tooltip = Nothing
              }
            , send <| SaveUserSetting ( "shortcutTrayOpen", Enc.bool newIsOpen )
            )

        -- === Ports ===
        Pull ->
            ( model, send <| PullData )

        Export ->
            let
                activeTree =
                    getTree vs.active model.workingTree.tree
                        |> Maybe.withDefault model.workingTree.tree
            in
            ( model
            , Export.command
                Exported
                (Metadata.getDocId model.metadata)
                (Metadata.getDocName model.metadata |> Maybe.withDefault "Untitled")
                model.exportSettings
                activeTree
                model.workingTree.tree
            )

        Exported docName (Ok bytes) ->
            let
                mime =
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"

                filename =
                    docName ++ ".docx"
            in
            ( model, Download.bytes filename mime bytes )

        Exported _ (Err _) ->
            ( model, Cmd.none )

        Incoming incomingMsg ->
            case incomingMsg of
                -- === Dialogs, Menus, Window State ===
                CancelCardConfirmed ->
                    ( { model | dirty = False }
                    , send <| SetDirty False
                    )
                        |> cancelCard

                -- === Database ===
                DataSaved dataIn ->
                    let
                        newData =
                            Data.success dataIn model.data
                    in
                    ( { model
                        | data = newData
                        , lastLocalSave = Data.lastCommitTime newData |> Maybe.map Time.millisToPosix
                        , dirty = False
                      }
                    , send <| SetDirty False
                    )

                DataReceived dataIn ->
                    dataReceived dataIn model

                NotFound ->
                    ( model, Route.replaceUrl (Session.navKey model.session) Route.Root )

                LocalStoreLoaded dataIn ->
                    let
                        ( newViewState, maybeActivateCmd ) =
                            case Json.decodeValue (Json.field "last-actives" (Json.list Json.string)) dataIn of
                                Ok (lastActive :: activePast) ->
                                    ( { vs | active = lastActive, activePast = activePast }
                                    , activate lastActive True
                                    )

                                Ok [] ->
                                    ( vs, identity )

                                Err _ ->
                                    ( vs, identity )

                        newTheme =
                            case Json.decodeValue Theme.decoder dataIn of
                                Ok decodedTheme ->
                                    decodedTheme

                                Err _ ->
                                    Default
                    in
                    ( { model | viewState = newViewState, theme = newTheme }, Cmd.none )
                        |> maybeActivateCmd

                MetadataSynced json ->
                    case Json.decodeValue Metadata.decoder json of
                        Ok metadata ->
                            ( { model | metadata = metadata, titleField = Metadata.getDocName metadata }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                MetadataSaved json ->
                    case Json.decodeValue Metadata.decoder json of
                        Ok metadata ->
                            ( { model | metadata = metadata, titleField = Metadata.getDocName metadata }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                MetadataSaveError ->
                    ( { model | titleField = Nothing }, Cmd.none )

                GetDataToSave ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Cmd.none
                              --, send (SaveToDB ( Metadata.encode model.metadata, statusToValue model.status, Data.encode model.objects ))
                            )

                        _ ->
                            let
                                newTree =
                                    TreeStructure.update (TreeStructure.Upd vs.active model.field) model.workingTree
                            in
                            if newTree.tree /= model.workingTree.tree then
                                ( { model | workingTree = newTree }
                                , Cmd.none
                                )
                                    |> addToHistoryDo

                            else
                                ( model
                                , Cmd.none
                                  --, send (SaveToDB ( Metadata.encode model.metadata, statusToValue model.status, Data.encode model.objects ))
                                )

                SavedLocally time_ ->
                    ( { model
                        | lastLocalSave = time_
                      }
                    , Cmd.none
                    )

                SavedRemotely time ->
                    ( { model
                        | lastRemoteSave = Just time
                      }
                    , Cmd.none
                    )

                -- === DOM ===
                DragStarted dragId ->
                    let
                        newTree =
                            TreeStructure.update (TreeStructure.Rmv dragId) model.workingTree

                        draggedTree =
                            getTreeWithPosition dragId model.workingTree.tree
                    in
                    if List.isEmpty <| getChildren newTree.tree then
                        ( model, Cmd.none )

                    else
                        ( { model | workingTree = newTree, viewState = { vs | draggedTree = draggedTree } }, Cmd.none )

                DragExternalStarted ->
                    if model.viewState.viewMode == Normal then
                        ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = True } ) } }, Cmd.none )

                    else
                        ( model, Cmd.none )

                DropExternal dropText ->
                    case Tuple.second vs.dragModel |> .dropId of
                        Just dropId ->
                            let
                                modelNoDrag =
                                    { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = False } ) } }

                                baseModelCmdTuple =
                                    case dropId of
                                        Above cardId ->
                                            ( modelNoDrag, Cmd.none ) |> insertAbove cardId dropText

                                        Into cardId ->
                                            ( modelNoDrag, Cmd.none ) |> insertChild cardId dropText

                                        Below cardId ->
                                            ( modelNoDrag, Cmd.none ) |> insertBelow cardId dropText
                            in
                            baseModelCmdTuple
                                |> closeCard
                                |> addToHistory

                        Nothing ->
                            ( model, Cmd.none )

                FullscreenChanged isFullscreen ->
                    if vs.viewMode == FullscreenEditing && not isFullscreen then
                        exitFullscreen model

                    else
                        ( model, Cmd.none )

                Paste tree ->
                    normalMode model (pasteBelow vs.active tree)

                PasteInto tree ->
                    normalMode model (pasteInto vs.active tree)

                FieldChanged str ->
                    ( { model | field = str, dirty = True }, Cmd.none )

                TextCursor textCursorInfo ->
                    if model.textCursorInfo /= textCursorInfo then
                        ( { model | textCursorInfo = textCursorInfo }
                        , Cmd.none
                        )

                    else
                        ( model, Cmd.none )

                ClickedOutsideCard ->
                    if model.viewState.viewMode == Editing then
                        ( model, Cmd.none )
                            |> saveCardIfEditing
                            |> closeCard

                    else
                        ( model, Cmd.none )

                CheckboxClicked cardId checkboxNumber ->
                    case getTree cardId model.workingTree.tree of
                        Nothing ->
                            ( model, Cmd.none )

                        Just originalCard ->
                            let
                                checkboxes =
                                    Regex.fromStringWith { caseInsensitive = True, multiline = True }
                                        "\\[(x| )\\]"
                                        |> Maybe.withDefault Regex.never

                                checkboxReplacer { match, number } =
                                    case ( number == checkboxNumber, match ) of
                                        ( True, "[ ]" ) ->
                                            "[X]"

                                        ( True, "[x]" ) ->
                                            "[ ]"

                                        ( True, "[X]" ) ->
                                            "[ ]"

                                        _ ->
                                            match

                                newContent =
                                    originalCard.content
                                        |> Regex.replace checkboxes checkboxReplacer

                                newTree =
                                    TreeStructure.update (TreeStructure.Upd cardId newContent) model.workingTree
                            in
                            ( { model | workingTree = newTree, dirty = True }, Cmd.none )
                                |> addToHistory

                -- === UI ===
                StartTour ->
                    ( { model | modalState = VideoViewer VideoViewer.init }, Cmd.none )

                FontSelectorOpen fonts ->
                    ( { model | fonts = Fonts.setSystem fonts model.fonts, fontSelectorOpen = True }
                    , Cmd.none
                    )

                Keyboard shortcut ->
                    case shortcut of
                        "shift+enter" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )
                                        |> openCardFullscreen vs.active (getContent vs.active model.workingTree.tree)

                                _ ->
                                    ( model, Cmd.none )

                        "mod+enter" ->
                            saveAndStopEditing model

                        "mod+s" ->
                            saveCardIfEditing ( model, Cmd.none )

                        "enter" ->
                            case model.modalState of
                                FileSwitcher switcherModel ->
                                    case switcherModel.selectedDocument of
                                        Just docId ->
                                            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

                                        Nothing ->
                                            ( model, Cmd.none )

                                _ ->
                                    normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

                        "mod+backspace" ->
                            normalMode model (deleteCard vs.active)

                        "esc" ->
                            case ( model.modalState, model.headerMenu ) of
                                ( NoModal, HistoryView historyState ) ->
                                    ( { model | headerMenu = NoHeaderMenu }, Cmd.none )
                                        |> checkoutCommit historyState.start

                                ( NoModal, _ ) ->
                                    model |> intentCancelCard

                                _ ->
                                    ( { model | fileSearchField = "", modalState = NoModal }, Cmd.none )

                        "mod+j" ->
                            if model.viewState.viewMode == Normal then
                                insertBelow vs.active "" ( model, Cmd.none )

                            else
                                let
                                    ( beforeText, afterText ) =
                                        model.textCursorInfo.text
                                in
                                ( { model | field = beforeText }
                                , Cmd.none
                                )
                                    |> saveCardIfEditing
                                    |> insertBelow vs.active afterText
                                    |> setCursorPosition 0

                        "mod+down" ->
                            normalMode model (insertBelow vs.active "")

                        "mod+k" ->
                            if model.viewState.viewMode == Normal then
                                insertAbove vs.active "" ( model, Cmd.none )

                            else
                                let
                                    ( beforeText, afterText ) =
                                        model.textCursorInfo.text
                                in
                                ( { model | field = afterText }
                                , Cmd.none
                                )
                                    |> saveCardIfEditing
                                    |> insertAbove vs.active beforeText

                        "mod+up" ->
                            normalMode model (insertAbove vs.active "")

                        "mod+l" ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( { model | field = beforeText }
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> insertChild vs.active afterText
                                |> setCursorPosition 0

                        "mod+right" ->
                            normalMode model (insertChild vs.active "")

                        "mod+shift+j" ->
                            normalMode model (mergeDown vs.active)

                        "mod+shift+down" ->
                            normalMode model (mergeDown vs.active)

                        "mod+shift+k" ->
                            normalMode model (mergeUp vs.active)

                        "mod+shift+up" ->
                            normalMode model (mergeUp vs.active)

                        "h" ->
                            normalMode model (goLeft vs.active)

                        "left" ->
                            normalMode model (goLeft vs.active)

                        "j" ->
                            normalMode model (goDown vs.active)

                        "down" ->
                            case model.modalState of
                                NoModal ->
                                    case vs.viewMode of
                                        Normal ->
                                            ( model, Cmd.none )
                                                |> goDown vs.active

                                        FullscreenEditing ->
                                            {- check if at end
                                               if so, getNextInColumn and openCardFullscreen it
                                            -}
                                            ( model, Cmd.none )

                                        Editing ->
                                            ( model, Cmd.none )

                                FileSwitcher switcherModel ->
                                    ( { model | modalState = FileSwitcher (Doc.Switcher.down switcherModel) }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        "k" ->
                            normalMode model (goUp vs.active)

                        "up" ->
                            case model.modalState of
                                NoModal ->
                                    normalMode model (goUp vs.active)

                                FileSwitcher switcherModel ->
                                    ( { model | modalState = FileSwitcher (Doc.Switcher.up switcherModel) }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        "l" ->
                            normalMode model (goRight vs.active)

                        "right" ->
                            normalMode model (goRight vs.active)

                        "alt+up" ->
                            normalMode model (moveWithin vs.active -1)

                        "alt+k" ->
                            normalMode model (moveWithin vs.active -1)

                        "alt+down" ->
                            normalMode model (moveWithin vs.active 1)

                        "alt+j" ->
                            normalMode model (moveWithin vs.active 1)

                        "alt+left" ->
                            normalMode model (moveLeft vs.active)

                        "alt+h" ->
                            normalMode model (moveLeft vs.active)

                        "alt+right" ->
                            normalMode model (moveRight vs.active)

                        "alt+l" ->
                            normalMode model (moveRight vs.active)

                        "alt+shift+up" ->
                            normalMode model (moveWithin vs.active -5)

                        "alt+shift+down" ->
                            normalMode model (moveWithin vs.active 5)

                        "alt+pageup" ->
                            normalMode model (moveWithin vs.active -999999)

                        "alt+pagedown" ->
                            normalMode model (moveWithin vs.active 999999)

                        "home" ->
                            normalMode model (goToTopOfColumn vs.active)

                        "end" ->
                            normalMode model (goToBottomOfColumn vs.active)

                        "pageup" ->
                            normalMode model (goToTopOfGroup vs.active True)

                        "pagedown" ->
                            normalMode model (goToBottomOfGroup vs.active True)

                        "mod+x" ->
                            normalMode model (cut vs.active)

                        "mod+c" ->
                            normalMode model (copy vs.active)

                        "mod+z" ->
                            normalMode model (\( m, _ ) -> toggleHistory True m)
                                |> Tuple.mapSecond (\c -> Cmd.batch [ c, send <| HistorySlider -1 ])

                        "mod+shift+z" ->
                            normalMode model (\( m, _ ) -> toggleHistory True m)
                                |> Tuple.mapSecond (\c -> Cmd.batch [ c, send <| HistorySlider 1 ])

                        "mod+o" ->
                            case model.modalState of
                                FileSwitcher _ ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                _ ->
                                    model |> openSwitcher

                        "mod+b" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , send (TextSurround vs.active "**")
                                    )

                        "mod+i" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , send (TextSurround vs.active "*")
                                    )

                        "/" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-input")
                                    )

                                _ ->
                                    ( model
                                    , Cmd.none
                                    )

                        "w" ->
                            case ( vs.viewMode, model.modalState ) of
                                ( Normal, NoModal ) ->
                                    ( { model | modalState = Wordcount }, Cmd.none )

                                ( Normal, Wordcount ) ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        "?" ->
                            case ( vs.viewMode, model.modalState ) of
                                ( Normal, HelpScreen ) ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                ( Normal, _ ) ->
                                    ( { model | modalState = HelpScreen }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                WillPrint ->
                    ( { model | headerMenu = ExportPreview }, Cmd.none )

                -- === Misc ===
                RecvCollabState collabState ->
                    let
                        newCollabs =
                            if List.member collabState.uid (vs.collaborators |> List.map .uid) then
                                vs.collaborators
                                    |> List.map
                                        (\c ->
                                            if c.uid == collabState.uid then
                                                collabState

                                            else
                                                c
                                        )

                            else
                                collabState :: vs.collaborators

                        newTree =
                            case collabState.mode of
                                CollabEditing editId ->
                                    TreeStructure.update (TreeStructure.Upd editId collabState.field) model.workingTree

                                _ ->
                                    model.workingTree
                    in
                    ( { model
                        | workingTree = newTree
                        , viewState = { vs | collaborators = newCollabs }
                      }
                    , Cmd.none
                    )

                CollaboratorDisconnected uid ->
                    ( { model
                        | viewState =
                            { vs | collaborators = vs.collaborators |> List.filter (\c -> c.uid /= uid) }
                      }
                    , Cmd.none
                    )

        LogErr err ->
            ( model
            , send (ConsoleLogRequested err)
            )

        NoOp ->
            ( model
            , Cmd.none
            )



-- === Card Activation ===


activate : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
activate tryId instant ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    if tryId == "0" then
        ( model
        , prevCmd
        )

    else
        let
            activeTree__ =
                getTree tryId model.workingTree.tree

            activeTree_ =
                case activeTree__ of
                    Just aTree ->
                        Just aTree

                    Nothing ->
                        getFirstCard model.workingTree.tree
        in
        case activeTree_ of
            Nothing ->
                ( model, prevCmd )

            Just activeTree ->
                let
                    newPast =
                        if tryId == vs.active then
                            vs.activePast

                        else
                            vs.active :: vs.activePast |> List.take 40

                    id =
                        activeTree.id

                    desc =
                        activeTree
                            |> getDescendants
                            |> List.map .id

                    anc =
                        getAncestors model.workingTree.tree activeTree []
                            |> List.map .id

                    newField =
                        activeTree.content

                    newModel =
                        { model
                            | viewState =
                                { vs
                                    | active = id
                                    , activePast = newPast
                                    , descendants = desc
                                    , ancestors = anc
                                }
                            , field = newField
                        }
                in
                case vs.viewMode of
                    FullscreenEditing ->
                        ( newModel
                        , Cmd.batch [ prevCmd, send <| ScrollFullscreenCards id ]
                        )

                    _ ->
                        let
                            scrollPositions =
                                getScrollPositions activeTree newPast model.workingTree.tree

                            colIdx =
                                getDepth 0 model.workingTree.tree activeTree.id
                        in
                        ( newModel
                        , Cmd.batch
                            [ prevCmd
                            , send
                                (ScrollCards (id :: newPast) scrollPositions colIdx instant)
                            ]
                        )
                            |> sendCollabState (CollabState model.uid (CollabActive id) "")


goLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goLeft id ( model, prevCmd ) =
    let
        targetId =
            getParent id model.workingTree.tree |> Maybe.withDefault defaultTree |> .id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goDown : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goDown id ( model, prevCmd ) =
    let
        targetId =
            case getNextInColumn id model.workingTree.tree of
                Nothing ->
                    id

                Just ntree ->
                    ntree.id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goUp : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goUp id ( model, prevCmd ) =
    let
        targetId =
            case getPrevInColumn id model.workingTree.tree of
                Nothing ->
                    id

                Just ntree ->
                    ntree.id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goRight id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        tree_ =
            getTree id model.workingTree.tree

        childrenIds =
            getChildren (tree_ |> Maybe.withDefault defaultTree)
                |> List.map .id

        firstChildId =
            childrenIds
                |> List.head
                |> Maybe.withDefault id

        prevActiveOfChildren =
            vs.activePast
                |> List.filter (\a -> List.member a childrenIds)
                |> List.head
                |> Maybe.withDefault firstChildId
    in
    case tree_ of
        Nothing ->
            ( model
            , prevCmd
            )

        Just _ ->
            if List.length childrenIds == 0 then
                ( model
                , prevCmd
                )

            else
                ( model
                , prevCmd
                )
                    |> activate prevActiveOfChildren False



-- === Card Editing  ===


saveAndStopEditing : Model -> ( Model, Cmd Msg )
saveAndStopEditing model =
    let
        vs =
            model.viewState
    in
    case vs.viewMode of
        Normal ->
            ( model, Cmd.none ) |> openCard vs.active (getContent vs.active model.workingTree.tree)

        Editing ->
            ( model, Cmd.none )
                |> saveCardIfEditing
                |> closeCard

        FullscreenEditing ->
            ( model, Cmd.none )
                |> saveCardIfEditing
                |> closeCard
                |> activate model.viewState.active True


saveCardIfEditing : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saveCardIfEditing ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    case vs.viewMode of
        Normal ->
            ( model
            , prevCmd
            )

        _ ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Upd vs.active model.field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                  }
                , prevCmd
                )
                    |> addToHistory

            else
                ( { model | dirty = False }
                , Cmd.batch [ prevCmd, send <| SetDirty False ]
                )


openCard : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCard id str ( model, prevCmd ) =
    let
        vs =
            model.viewState

        ( newViewMode, maybeScroll ) =
            if vs.viewMode == FullscreenEditing then
                ( FullscreenEditing, send <| ScrollFullscreenCards id )

            else
                ( Editing, Cmd.none )

        isLocked =
            vs.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)

        isHistoryView =
            case model.headerMenu of
                HistoryView _ ->
                    True

                _ ->
                    False
    in
    if isHistoryView then
        ( model
        , Cmd.batch [ prevCmd, send (Alert "Cannot edit while browsing version history.") ]
        )

    else if isLocked then
        ( model
        , Cmd.batch [ prevCmd, send (Alert "Card is being edited by someone else.") ]
        )

    else
        ( { model
            | viewState = { vs | active = id, viewMode = newViewMode }
            , field = str
          }
        , Cmd.batch [ prevCmd, focus id, maybeScroll ]
        )
            |> sendCollabState (CollabState model.uid (CollabEditing id) str)


openCardFullscreen : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCardFullscreen id str ( model, prevCmd ) =
    ( model, prevCmd )
        |> openCard id str
        |> (\( m, c ) ->
                let
                    vs =
                        m.viewState
                in
                ( { m | viewState = { vs | active = id, viewMode = FullscreenEditing }, field = str }
                , focus id
                )
           )


enterFullscreen : Model -> ( Model, Cmd Msg )
enterFullscreen model =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = FullscreenEditing } }
    , focus vs.active
    )


exitFullscreen : Model -> ( Model, Cmd Msg )
exitFullscreen model =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = Editing } }
    , Cmd.batch [ send <| SetField vs.active model.field, focus vs.active ]
    )


closeCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
closeCard ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = Normal }, field = "" }, prevCmd )


deleteCard : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
deleteCard id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        isLocked =
            vs.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)

        parent_ =
            getParent id model.workingTree.tree

        prev_ =
            getPrevInColumn id model.workingTree.tree

        next_ =
            getNextInColumn id model.workingTree.tree

        ( nextToActivate, isLastChild ) =
            case ( parent_, prev_, next_ ) of
                ( _, Just prev, _ ) ->
                    ( prev.id, False )

                ( _, Nothing, Just next ) ->
                    ( next.id, False )

                ( Just parent, Nothing, Nothing ) ->
                    ( parent.id, parent.id == "0" )

                ( Nothing, Nothing, Nothing ) ->
                    ( "0", True )
    in
    if isLocked then
        ( model
        , send (Alert "Card is being edited by someone else.")
        )

    else if isLastChild then
        ( model
        , send (Alert "Cannot delete last card.")
        )

    else
        ( { model
            | workingTree = TreeStructure.update (TreeStructure.Rmv id) model.workingTree
            , dirty = True
          }
        , Cmd.batch [ prevCmd, send <| SetDirty True ]
        )
            |> activate nextToActivate False
            |> addToHistory


goToTopOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToTopOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getFirstInColumn id model.workingTree.tree) False


goToBottomOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToBottomOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getLastInColumn id model.workingTree.tree) False


goToTopOfGroup : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToTopOfGroup id fallToNextGroup ( model, prevCmd ) =
    let
        topSibling =
            case
                getSiblings id model.workingTree.tree
                    |> List.head
            of
                Nothing ->
                    id

                Just lastSiblingTree ->
                    lastSiblingTree.id

        targetId =
            if topSibling == id && fallToNextGroup then
                case getPrevInColumn id model.workingTree.tree of
                    Nothing ->
                        topSibling

                    Just previousColumnTree ->
                        previousColumnTree.id

            else
                topSibling
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goToBottomOfGroup : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToBottomOfGroup id fallToNextGroup ( model, prevCmd ) =
    let
        bottomSibling =
            case
                getSiblings id model.workingTree.tree
                    |> List.reverse
                    |> List.head
            of
                Nothing ->
                    id

                Just lastSiblingTree ->
                    lastSiblingTree.id

        targetId =
            if bottomSibling == id && fallToNextGroup then
                case getNextInColumn id model.workingTree.tree of
                    Nothing ->
                        bottomSibling

                    Just nextColumnTree ->
                        nextColumnTree.id

            else
                bottomSibling
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


cancelCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cancelCard ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    ( { model
        | viewState = { vs | viewMode = Normal }
        , field = ""
      }
    , prevCmd
    )
        |> sendCollabState (CollabState model.uid (CollabActive vs.active) "")
        |> activate vs.active True


intentCancelCard : Model -> ( Model, Cmd Msg )
intentCancelCard model =
    let
        vs =
            model.viewState

        originalContent =
            getContent vs.active model.workingTree.tree
    in
    case vs.viewMode of
        Normal ->
            ( model
            , Cmd.none
            )

        _ ->
            ( model
            , send (ConfirmCancelCard vs.active originalContent (tr (Session.language model.session) AreYouSureCancel))
            )



-- === Card Insertion  ===


insert : String -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insert pid pos initText ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (Session.seed model.session)

        newIdString =
            "node-" ++ (newId |> String.fromInt)
    in
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Ins newIdString initText pid pos) model.workingTree
        , session = Session.setSeed newSeed model.session
      }
    , prevCmd
    )
        |> openCard newIdString initText
        |> activate newIdString False


insertRelative : String -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertRelative id delta initText ( model, prevCmd ) =
    let
        idx =
            getIndex id model.workingTree.tree |> Maybe.withDefault 999999

        pid_ =
            getParent id model.workingTree.tree |> Maybe.map .id
    in
    case pid_ of
        Just pid ->
            ( model
            , prevCmd
            )
                |> insert pid (idx + delta) initText

        Nothing ->
            ( model
            , prevCmd
            )


insertAbove : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertAbove id initText tup =
    insertRelative id 0 initText tup


insertBelow : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertBelow id initText ( model, prevCmd ) =
    insertRelative id 1 initText ( model, prevCmd )


insertChild : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertChild id initText ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> insert id 999999 initText


mergeUp : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mergeUp id ( model, prevCmd ) =
    let
        currentTree_ =
            getTree id model.workingTree.tree

        prevTree_ =
            getPrevInColumn id model.workingTree.tree
    in
    case ( currentTree_, prevTree_ ) of
        ( Just currentTree, Just prevTree ) ->
            let
                mergedTree =
                    model.workingTree
                        |> TreeStructure.update (TreeStructure.Mrg currentTree prevTree True)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate prevTree.id False
                |> addToHistory

        _ ->
            ( model, prevCmd )


mergeDown : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mergeDown id ( model, prevCmd ) =
    let
        currentTree_ =
            getTree id model.workingTree.tree

        nextTree_ =
            getNextInColumn id model.workingTree.tree
    in
    case ( currentTree_, nextTree_ ) of
        ( Just currentTree, Just nextTree ) ->
            let
                mergedTree =
                    model.workingTree
                        |> TreeStructure.update (TreeStructure.Mrg currentTree nextTree False)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate nextTree.id False
                |> addToHistory

        _ ->
            ( model, prevCmd )


setCursorPosition : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setCursorPosition pos ( model, prevCmd ) =
    ( model, Cmd.batch [ prevCmd, send (SetCursorPosition pos) ] )



-- === Card Moving  ===


move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Mov subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> activate subtree.id False
        |> addToHistory


moveWithin : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveWithin id delta ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        pid_ =
            getParent id model.workingTree.tree
                |> Maybe.map .id

        refIdx_ =
            getIndex id model.workingTree.tree
    in
    case ( tree_, pid_, refIdx_ ) of
        ( Just tree, Just pid, Just refIdx ) ->
            ( model
            , prevCmd
            )
                |> move tree pid (refIdx + delta |> Basics.max 0)

        _ ->
            ( model
            , prevCmd
            )


moveLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveLeft id ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        parentId =
            getParent id model.workingTree.tree
                |> Maybe.map .id
                |> Maybe.withDefault "invalid"

        parentIdx_ =
            getIndex parentId model.workingTree.tree

        grandparentId_ =
            getParent parentId model.workingTree.tree
                |> Maybe.map .id
    in
    case ( tree_, grandparentId_, parentIdx_ ) of
        ( Just tree, Just gpId, Just refIdx ) ->
            ( model
            , prevCmd
            )
                |> move tree gpId (refIdx + 1)

        _ ->
            ( model
            , prevCmd
            )


moveRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveRight id ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        prev_ =
            getPrev id model.workingTree.tree
                |> Maybe.map .id
    in
    case ( tree_, prev_ ) of
        ( Just tree, Just prev ) ->
            ( model
            , prevCmd
            )
                |> move tree prev 999999

        _ ->
            ( model
            , prevCmd
            )



-- === Card Cut/Copy/Paste ===


cut : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cut id ( model, prevCmd ) =
    let
        parent_ =
            getParent id model.workingTree.tree

        prev_ =
            getPrevInColumn id model.workingTree.tree

        next_ =
            getNextInColumn id model.workingTree.tree

        isLastChild =
            case ( parent_, prev_, next_ ) of
                ( Just parent, Nothing, Nothing ) ->
                    parent.id == "0"

                _ ->
                    False
    in
    if isLastChild then
        ( model
        , send (Alert "Cannot cut last card")
        )

    else
        ( model, prevCmd )
            |> copy id
            |> deleteCard id


copy : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
copy id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        copiedTree_ =
            getTree id model.workingTree.tree
    in
    ( { model
        | viewState = { vs | clipboardTree = copiedTree_ }
      }
    , Cmd.batch
        [ prevCmd
        , case copiedTree_ of
            Just tree ->
                send <| CopyCurrentSubtree <| treeToValue tree

            Nothing ->
                Cmd.none
        ]
    )


paste : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
paste subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Paste subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> activate subtree.id False
        |> addToHistory


pasteBelow : String -> Tree -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteBelow id copiedTree ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (Session.seed model.session)

        treeToPaste =
            TreeStructure.renameNodes (newId |> String.fromInt) copiedTree

        pid =
            (getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0"

        pos =
            (getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1
    in
    ( { model | session = Session.setSeed newSeed model.session }
    , prevCmd
    )
        |> paste treeToPaste pid pos


pasteInto : String -> Tree -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteInto id copiedTree ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (Session.seed model.session)

        treeToPaste =
            TreeStructure.renameNodes (newId |> String.fromInt) copiedTree
    in
    ( { model | session = Session.setSeed newSeed model.session }
    , prevCmd
    )
        |> paste treeToPaste id 999999



-- === Welcome Checklist  ===
-- === History ===


checkoutCommit : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkoutCommit commitSha ( model, prevCmd ) =
    let
        newTree_ =
            Data.checkout commitSha model.data
    in
    case newTree_ of
        Just newTree ->
            ( { model
                | workingTree = TreeStructure.setTree newTree model.workingTree
              }
            , Cmd.none
            )
                |> activate model.viewState.active False

        Nothing ->
            ( model
            , prevCmd
            )


toggleHistory : Bool -> Model -> ( Model, Cmd msg )
toggleHistory isOpen model =
    case ( isOpen, Data.head "heads/master" model.data ) of
        ( True, Just refObj ) ->
            ( { model | headerMenu = HistoryView { start = refObj.value, currentView = refObj.value }, tooltip = Nothing }, Cmd.none )

        _ ->
            ( { model | headerMenu = NoHeaderMenu }, Cmd.none )



-- History


type Direction
    = Forward
    | Backward


type HistoryState
    = Closed
    | From String


addToHistoryDo : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryDo ( { workingTree, session } as model, prevCmd ) =
    let
        author =
            session |> Session.name |> Maybe.withDefault "unknown" |> (\a -> "<" ++ a ++ ">")

        commitReq_ =
            Data.requestCommit workingTree.tree author model.data (Metadata.encode model.metadata)
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


addToHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistory ( model, prevCmd ) =
    update (ThrottledCommit (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])



-- === Files ===


dataReceived : Json.Value -> Model -> ( Model, Cmd Msg )
dataReceived dataIn model =
    case Data.received dataIn ( model.data, model.workingTree.tree ) of
        Just { newModel, newTree } ->
            let
                newWorkingTree =
                    TreeStructure.setTreeWithConflicts (Data.conflictList newModel) newTree model.workingTree

                startingWordcount =
                    countWords (treeToMarkdownString False newTree)
            in
            ( { model
                | data = newModel
                , loading = False
                , workingTree = newWorkingTree
                , lastLocalSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
                , lastRemoteSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
                , startingWordcount = startingWordcount
              }
            , Cmd.none
            )
                |> (if model.loading then
                        activate model.viewState.active True

                    else
                        identity
                   )

        Nothing ->
            ( model, Cmd.none )


sendCollabState : CollabState -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendCollabState collabState ( model, prevCmd ) =
    ( model
    , Cmd.batch [ prevCmd, send (SocketSend collabState) ]
    )


openSwitcher : Model -> ( Model, Cmd Msg )
openSwitcher model =
    ( { model
        | modalState =
            FileSwitcher
                { currentDocument = model.metadata
                , selectedDocument = Just (Metadata.getDocId model.metadata)
                , searchField = model.fileSearchField
                , docList = Session.documents model.session
                }
      }
    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "switcher-input")
    )


closeSwitcher : Model -> ( Model, Cmd Msg )
closeSwitcher model =
    ( { model | modalState = NoModal }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        sidebarOpen =
            Session.fileMenuOpen model.session
    in
    if model.loading then
        UI.viewLoadingSpinner sidebarOpen

    else
        viewLoaded model


viewLoaded : Model -> Html Msg
viewLoaded model =
    let
        language =
            Session.language model.session

        isMac =
            Session.isMac model.session
    in
    case Data.conflictList model.data of
        [] ->
            if model.viewState.viewMode == FullscreenEditing then
                lazy3 Fullscreen.view
                    { language = language
                    , isMac = isMac
                    , dirty = model.dirty
                    , model = model.workingTree
                    , lastLocalSave = model.lastLocalSave
                    , lastRemoteSave = model.lastRemoteSave
                    , currentTime = Session.currentTime model.session
                    }
                    model.field
                    model.viewState
                    |> Html.map FullscreenMsg

            else
                let
                    activeTree_ =
                        getTree model.viewState.active model.workingTree.tree

                    mobileBtnMsg shortcut =
                        Incoming (Keyboard shortcut)

                    exportViewOk =
                        lazy4 exportView
                            { export = Export
                            , printRequested = PrintRequested
                            , tooltipRequested = TooltipRequested
                            , tooltipClosed = TooltipClosed
                            }
                            model.exportSettings

                    maybeExportView =
                        case ( model.headerMenu, activeTree_, model.exportSettings ) of
                            ( ExportPreview, Just activeTree, _ ) ->
                                exportViewOk activeTree model.workingTree.tree

                            ( ExportPreview, Nothing, ( ExportEverything, _ ) ) ->
                                exportViewOk defaultTree model.workingTree.tree

                            ( ExportPreview, Nothing, _ ) ->
                                exportViewError "No card selected, cannot preview document"

                            _ ->
                                text ""

                    cardTitleReplacer ( id, inputString ) =
                        case String.lines inputString of
                            firstLine :: _ ->
                                ( id
                                , firstLine
                                    |> String.trim
                                    |> (\str ->
                                            if String.isEmpty str then
                                                "(empty)"

                                            else
                                                str
                                       )
                                )

                            [] ->
                                ( id, "(empty)" )

                    cardTitles =
                        case activeTree_ of
                            Just activeTree ->
                                (getAncestors model.workingTree.tree activeTree []
                                    |> List.map (\t -> ( t.id, t.content ))
                                    |> List.drop 1
                                )
                                    ++ [ ( activeTree.id, activeTree.content ) ]
                                    |> List.map cardTitleReplacer

                            Nothing ->
                                []
                in
                div
                    [ id "app-root", applyTheme model.theme ]
                    ([ lazy4 treeView (Session.language model.session) (Session.isMac model.session) model.viewState model.workingTree
                     , UI.viewHeader
                        { noOp = NoOp
                        , titleFocused = TitleFocused
                        , titleFieldChanged = TitleFieldChanged
                        , titleEdited = TitleEdited
                        , titleEditCanceled = TitleEditCanceled
                        , tooltipRequested = TooltipRequested
                        , tooltipClosed = TooltipClosed
                        , toggledHistory = HistoryToggled
                        , checkoutCommit = CheckoutCommit
                        , restore = Restore
                        , cancelHistory = CancelHistory
                        , toggledDocSettings = DocSettingsToggled (not <| model.headerMenu == Settings)
                        , wordCountClicked = WordcountModalOpened
                        , themeChanged = ThemeChanged
                        , toggledExport = ExportPreviewToggled (not <| model.headerMenu == ExportPreview)
                        , exportSelectionChanged = ExportSelectionChanged
                        , exportFormatChanged = ExportFormatChanged
                        , export = Export
                        , printRequested = PrintRequested
                        , toggledUpgradeModal = ToggledUpgradeModal
                        }
                        (Metadata.getDocName model.metadata)
                        model
                     , if (not << List.isEmpty) cardTitles then
                        UI.viewBreadcrumbs Activate cardTitles

                       else
                        text ""
                     , UI.viewSidebar
                        model.session
                        { sidebarStateChanged = SidebarStateChanged
                        , noOp = NoOp
                        , clickedNew = TemplateSelectorOpened
                        , tooltipRequested = TooltipRequested
                        , tooltipClosed = TooltipClosed
                        , clickedSwitcher = SwitcherOpened
                        , clickedHelp = ToggledHelpMenu True
                        , toggledShortcuts = ShortcutTrayToggle
                        , clickedEmailSupport = ClickedEmailSupport
                        , clickedShowVideos = ClickedShowVideos
                        , languageMenuRequested = LanguageMenuRequested
                        , logout = LogoutRequested
                        , toggledAccount = ToggledAccountMenu
                        , fileSearchChanged = FileSearchChanged
                        , changeSortBy = SortByChanged
                        , contextMenuOpened = SidebarContextClicked
                        , languageChanged = LanguageChanged
                        , fullscreenRequested = FullscreenRequested
                        }
                        model.metadata
                        (Session.sortBy model.session)
                        model.fileSearchField
                        (Session.documents model.session)
                        (Session.name model.session |> Maybe.withDefault "" {- TODO -})
                        (case model.modalState of
                            SidebarContextMenu docId _ ->
                                Just docId

                            _ ->
                                Nothing
                        )
                        model.sidebarMenuState
                        model.sidebarState
                     , maybeExportView
                     ]
                        ++ UI.viewShortcuts
                            { toggledShortcutTray = ShortcutTrayToggle, tooltipRequested = TooltipRequested, tooltipClosed = TooltipClosed }
                            language
                            (Session.shortcutTrayOpen model.session)
                            (Session.isMac model.session)
                            model.workingTree.tree.children
                            model.textCursorInfo
                            model.viewState
                        ++ [ viewSearchField SearchFieldUpdated model
                           , viewMobileButtons
                                { edit = mobileBtnMsg "mod+enter"
                                , save = mobileBtnMsg "mod+enter"
                                , cancel = mobileBtnMsg "esc"
                                , plusDown = mobileBtnMsg "mod+down"
                                , plusUp = mobileBtnMsg "mod+up"
                                , plusRight = mobileBtnMsg "mod+right"
                                , navLeft = mobileBtnMsg "left"
                                , navUp = mobileBtnMsg "up"
                                , navDown = mobileBtnMsg "down"
                                , navRight = mobileBtnMsg "right"
                                }
                                (model.viewState.viewMode /= Normal)
                           , div [ id "loading-overlay" ] []
                           , div [ id "preloader" ] []
                           , model.tooltip |> Maybe.map UI.viewTooltip |> Maybe.withDefault (text "")
                           ]
                        ++ viewModal (Session.language model.session) model
                    )

        conflicts ->
            let
                bgString =
                    """
repeating-linear-gradient(-45deg
, rgba(255,255,255,0.02)
, rgba(255,255,255,0.02) 15px
, rgba(0,0,0,0.025) 15px
, rgba(0,0,0,0.06) 30px
)
          """
            in
            div
                [ id "app-root"
                , style "background" bgString
                , style "position" "absolute"
                , style "width" "100%"
                , style "height" "100%"
                ]
                [ ul [ class "conflicts-list" ]
                    (List.map (viewConflict SetSelection Resolve) conflicts)
                , lazy4 treeView (Session.language model.session) (Session.isMac model.session) model.viewState model.workingTree
                ]


treeView : Language -> Bool -> ViewState -> TreeStructure.Model -> Html Msg
treeView lang isMac vstate model =
    let
        searchFilter term_ cols =
            case term_ of
                Just term ->
                    let
                        hasTerm tree =
                            term
                                |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
                                |> Maybe.withDefault Regex.never
                                |> (\t -> Regex.contains t tree.content)
                    in
                    cols
                        |> List.map (\c -> List.map (\g -> List.filter hasTerm g) c)

                Nothing ->
                    cols

        columnsFiltered =
            model.columns
                |> searchFilter vstate.searchField
                |> List.drop 1

        getViewArgs c =
            let
                editing_ =
                    case vstate.viewMode of
                        Normal ->
                            Normal

                        Editing ->
                            if c |> List.concat |> List.map .id |> List.member vstate.active then
                                Editing

                            else
                                Normal

                        FullscreenEditing ->
                            -- TODO : Impossible state
                            Normal
            in
            VisibleViewState
                vstate.active
                editing_
                vstate.descendants
                vstate.ancestors
                vstate.dragModel
                vstate.collaborators
                lang
                isMac

        columns =
            columnsFiltered
                |> List.map (\c -> lazy2 viewColumn (getViewArgs c) c)
    in
    div
        [ id "document"
        ]
        [ div [ class "left-padding-column" ] []
        , div [ id "column-container" ]
            columns
        , div [ class "right-padding-column" ] []
        ]


viewColumn : VisibleViewState -> Column -> Html Msg
viewColumn vstate col =
    let
        buffer =
            [ div [ class "buffer" ] [] ]
    in
    div
        [ class "column" ]
        (buffer
            ++ List.map (lazy2 viewGroup vstate) col
            ++ buffer
        )


viewGroup : VisibleViewState -> Group -> Html Msg
viewGroup vstate xs =
    let
        firstChild =
            xs
                |> List.head
                |> Maybe.withDefault defaultTree
                |> .id

        lastChild =
            xs
                |> List.reverse
                |> List.head
                |> Maybe.withDefault defaultTree
                |> .id

        hasActive =
            xs
                |> List.map .id
                |> List.member vstate.active

        isActiveDescendant =
            vstate.descendants
                |> List.member firstChild

        viewFunction t =
            let
                isActive =
                    t.id == vstate.active

                isAncestor =
                    List.member t.id vstate.ancestors

                isEditing =
                    case vstate.viewMode of
                        Editing ->
                            t.id == vstate.active

                        Normal ->
                            False

                        FullscreenEditing ->
                            -- TODO : Impossible state
                            False

                isLast =
                    t.id == lastChild

                collabsEditingCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == CollabEditing t.id)
                        |> List.map .uid

                collabsOnCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == CollabActive t.id || c.mode == CollabEditing t.id)
                        |> List.map .uid
            in
            if isActive && not isEditing then
                ( t.id, lazy8 viewCardActive vstate.language t.id t.content (hasChildren t) isLast collabsOnCard collabsEditingCard vstate.dragModel )

            else if isEditing then
                ( t.id, viewCardEditing vstate.language t.id t.content (hasChildren t) vstate.isMac )

            else
                ( t.id, lazy7 viewCardOther t.id t.content isEditing (hasChildren t) isAncestor isLast vstate.dragModel )
    in
    Keyed.node "div"
        [ classList
            [ ( "group", True )
            , ( "has-active", hasActive )
            , ( "active-descendant", isActiveDescendant )
            ]
        ]
        (List.map viewFunction xs
            ++ (if isActiveDescendant then
                    [ ( "fillet-top-left", UI.fillet "top-left" )
                    , ( "fillet-bottom-left", UI.fillet "bottom-left" )
                    , ( "fillet-top-right", UI.fillet "top-right" )
                    , ( "fillet-bottom-right", UI.fillet "bottom-right" )
                    ]

                else
                    []
               )
        )


viewCardOther : String -> String -> Bool -> Bool -> Bool -> Bool -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardOther cardId content isEditing isParent isAncestor isLast dragModels =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "ancestor", isAncestor )
            , ( "has-children", isParent )
            ]
        ]
        ((if not isEditing then
            [ div ([ class "drag-region", title "Drag to move" ] ++ DragDrop.draggable DragDropMsg cardId) [ div [ class "handle" ] [] ] ]

          else
            []
         )
            ++ dropRegions cardId isEditing isLast dragModels
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               ]
        )


viewCardActive : Language -> String -> String -> Bool -> Bool -> List String -> List String -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardActive lang cardId content isParent isLast collabsOnCard collabsEditingCard dragModels =
    let
        buttons =
            [ div [ class "flex-row card-top-overlay" ]
                [ span
                    [ class "card-btn ins-above"
                    , title <| tr lang InsertAboveTitle
                    , onClick (InsertAbove cardId)
                    ]
                    [ text "+" ]
                ]
            , div [ class "flex-column card-right-overlay" ]
                [ span
                    [ class "card-btn delete"
                    , title <| tr lang DeleteCardTitle
                    , onClick (DeleteCard cardId)
                    ]
                    []
                , span
                    [ class "card-btn ins-right"
                    , title <| tr lang InsertChildTitle
                    , onClick (InsertChild cardId)
                    ]
                    [ text "+" ]
                , span
                    [ class "card-btn edit"
                    , title <| tr lang EditCardTitle
                    , onClick (OpenCard cardId content)
                    ]
                    []
                ]
            , div [ class "flex-row card-bottom-overlay" ]
                [ span
                    [ class "card-btn ins-below"
                    , title <| tr lang InsertBelowTitle
                    , onClick (InsertBelow cardId)
                    ]
                    [ text "+" ]
                ]
            , viewIf isParent <| UI.fillet "top-right"
            , viewIf isParent <| UI.fillet "bottom-right"
            ]
    in
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "collab-active", not (List.isEmpty collabsOnCard) )
            , ( "collab-editing", not (List.isEmpty collabsEditingCard) )
            , ( "has-children", isParent )
            ]
        ]
        ([ div ([ class "drag-region", title "Drag to move" ] ++ DragDrop.draggable DragDropMsg cardId) [ div [ class "handle" ] [] ] ]
            ++ buttons
            ++ dropRegions cardId False isLast dragModels
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardEditing : Language -> String -> String -> Bool -> Bool -> Html Msg
viewCardEditing lang cardId content isParent isMac =
    let
        ctrlOrCmd =
            ctrlOrCmdText isMac
    in
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "editing", True )
            , ( "has-children", isParent )
            ]
        , attribute "data-cloned-content" content
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , attribute "data-private" "lipsum"
            , onInput <| UpdateActiveField cardId
            , value content
            ]
            []
        , div [ class "flex-column card-right-overlay" ]
            [ div
                [ class "fullscreen-card-btn"
                , title "Edit in Fullscreen"
                , onClick EditToFullscreenMode
                ]
                [ AntIcons.fullscreenOutlined [ Attributes.width 16, Attributes.height 16 ] ]
            , div
                [ class "card-btn save"
                , title <| tr lang SaveChangesTitle
                , onClick SaveAndCloseCard
                ]
                []
            ]
        ]



-- HELPERS


hasChildren : Tree -> Bool
hasChildren { children } =
    case children of
        Children c ->
            (c
                |> List.length
            )
                /= 0


dropRegions : String -> Bool -> Bool -> ( DragDrop.Model String DropId, DragExternalModel ) -> List (Html Msg)
dropRegions cardId isEditing isLast ( dragModel, dragExternalModel ) =
    let
        dragId_ =
            DragDrop.getDragId dragModel

        dropId_ =
            DragDrop.getDropId dragModel

        dropDiv str dId =
            div
                ([ classList
                    [ ( "drop-region-" ++ str, True )
                    , ( "drop-hover", dropId_ == Just dId )
                    ]
                 ]
                    ++ DragDrop.droppable DragDropMsg dId
                )
                []

        dropDivExternal str dId =
            div
                [ classList
                    [ ( "drop-region-" ++ str, True )
                    , ( "drop-hover", .dropId dragExternalModel == Just dId )
                    ]
                , onWithOptions "dragenter" { stopPropagation = True, preventDefault = True } <| Json.succeed <| DragExternal <| DragEnter dId
                , onWithOptions "dragleave" { stopPropagation = True, preventDefault = True } <| Json.succeed <| DragExternal <| DragLeave dId
                ]
                []
    in
    case ( dragId_, dragExternalModel.isDragging, isEditing ) of
        ( Just _, _, False ) ->
            [ dropDiv "above" (Above cardId)
            , dropDiv "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDiv "below" (Below cardId) ]

                    else
                        []
                   )

        ( Nothing, True, False ) ->
            [ dropDivExternal "above" (Above cardId)
            , dropDivExternal "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDivExternal "below" (Below cardId) ]

                    else
                        []
                   )

        _ ->
            []


viewContent : String -> String -> Html Msg
viewContent cardId content =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }

        processedContent =
            let
                checkboxes =
                    Regex.fromStringWith { caseInsensitive = True, multiline = True }
                        "\\[(x| )\\]"
                        |> Maybe.withDefault Regex.never

                openAddDiff =
                    Regex.fromString "{\\+\\+" |> Maybe.withDefault Regex.never

                closeAddDiff =
                    Regex.fromString "\\+\\+}" |> Maybe.withDefault Regex.never

                openDelDiff =
                    Regex.fromString "{--" |> Maybe.withDefault Regex.never

                closeDelDiff =
                    Regex.fromString "--}" |> Maybe.withDefault Regex.never

                checkboxReplacer { match, number } =
                    let
                        checkState =
                            if match == "[x]" || match == "[X]" then
                                "checked"

                            else
                                ""
                    in
                    "<input type='checkbox'"
                        ++ checkState
                        ++ " onClick='checkboxClicked(\""
                        ++ cardId
                        ++ "\", "
                        ++ String.fromInt number
                        ++ ")'/>"
            in
            content
                |> Regex.replace openAddDiff (\_ -> "<ins class='diff'>")
                |> Regex.replace closeAddDiff (\_ -> "</ins>")
                |> Regex.replace openDelDiff (\_ -> "<del class='diff'>")
                |> Regex.replace closeDelDiff (\_ -> "</del>")
                |> Regex.replace checkboxes checkboxReplacer
    in
    Markdown.toHtmlWith options
        [ attribute "data-private" "lipsum" ]
        processedContent


collabsSpan : List String -> List String -> Html Msg
collabsSpan collabsOnCard collabsEditingCard =
    let
        collabsString =
            collabsOnCard
                |> List.map
                    (\c ->
                        if List.member c collabsEditingCard then
                            c ++ " is editing"

                        else
                            c
                    )
                |> String.join ", "
    in
    span [ class "collaborators" ] [ text collabsString ]


viewModal : Language -> Model -> List (Html Msg)
viewModal language model =
    case model.modalState of
        NoModal ->
            [ text "" ]

        FileSwitcher switcherModel ->
            Doc.Switcher.view SwitcherClosed FileSearchChanged switcherModel

        --model.metadata model.fileSearchField (Session.documents model.session)
        SidebarContextMenu docId ( x, y ) ->
            [ div [ onClick ModalClosed, id "sidebar-context-overlay" ] []
            , div
                [ id "sidebar-context-menu"
                , style "top" (String.fromFloat y ++ "px")
                , style "left" (String.fromFloat x ++ "px")
                ]
                [ div [ onClick (DuplicateDoc docId), class "context-menu-item" ]
                    [ AntIcons.copyOutlined [ Svg.Attributes.class "icon" ], text "Duplicate Tree" ]
                , div [ onClick (DeleteDoc docId), class "context-menu-item" ]
                    [ AntIcons.deleteOutlined [ Svg.Attributes.class "icon" ], text "Delete Tree" ]
                ]
            ]

        TemplateSelector ->
            UI.viewTemplateSelector language
                { modalClosed = ModalClosed
                , importBulkClicked = ImportBulkClicked
                , importMarkdownRequested = ImportMarkdownRequested
                , importOpmlRequested = ImportOpmlRequested
                , importJSONRequested = ImportJSONRequested
                }

        HelpScreen ->
            HelpScreen.view (Session.isMac model.session)
                { closeModal = ModalClosed
                , showVideoTutorials = VideoViewerOpened
                , showWidget = ClickedShowWidget
                , contactSupport = ClickedEmailSupport
                }

        VideoViewer videoViewerState ->
            VideoViewer.view language ModalClosed VideoViewerMsg videoViewerState

        Wordcount ->
            UI.viewWordCount model { modalClosed = ModalClosed }

        ImportModal modalModel ->
            ImportModal.view language modalModel
                |> List.map (Html.map ImportModalMsg)

        ContactForm contactFormModel ->
            ContactForm.view language
                { closeMsg = ModalClosed
                , submitMsg = ContactFormSubmitted
                , tagger = ContactFormMsg contactFormModel
                , copyEmail = CopyEmailClicked
                }
                contactFormModel

        UpgradeModal ->
            case Session.upgradeModel model.session of
                Just upgradeModel ->
                    Upgrade.view upgradeModel
                        |> List.map (Html.map UpgradeModalMsg)

                Nothing ->
                    []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Incoming.subscribe Incoming LogErr
        , Import.Incoming.importComplete
            (\docId_ ->
                case docId_ of
                    Just docId ->
                        ImportJSONCompleted docId

                    Nothing ->
                        ImportBulkCompleted
            )
        , DocList.subscribe ReceivedDocuments
        , case model.modalState of
            ImportModal importModalModel ->
                ImportModal.subscriptions importModalModel
                    |> Sub.map ImportModalMsg

            _ ->
                Sub.none
        , Session.userSettingsChange SettingsChanged
        , Session.loginChanges LoginStateChanged (Session.navKey model.session)
        , if model.dirty then
            Time.every (241 * 1000) (always AutoSave)

          else
            Sub.none
        , Time.every (9 * 1000) TimeUpdate
        , Time.every (23 * 1000) (always Pull)
        ]



-- HELPERS


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus ("card-edit-" ++ id))


normalMode : Model -> (( Model, Cmd Msg ) -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
normalMode model operation =
    ( model
    , Cmd.none
    )
        |> (case ( model.viewState.viewMode, model.modalState ) of
                ( Normal, NoModal ) ->
                    operation

                _ ->
                    identity
           )


onWithOptions :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decoder msg
    -> Attribute msg
onWithOptions name { stopPropagation, preventDefault } decoder =
    decoder
        |> Json.map (\msg -> { message = msg, stopPropagation = stopPropagation, preventDefault = preventDefault })
        |> custom name
