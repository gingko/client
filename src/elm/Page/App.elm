module Page.App exposing (Model, Msg, getTitle, init, isDirty, subscriptions, toSession, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (sortByEncoder)
import Doc.ContactForm as ContactForm
import Doc.Data as Data
import Doc.HelpScreen as HelpScreen
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata
import Doc.Switcher
import Doc.UI as UI
import Doc.VideoViewer as VideoViewer
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, classList, id, style)
import Html.Events exposing (onClick)
import Http
import Import.Bulk.UI as ImportModal
import Import.Incoming
import Import.Opml
import Import.Single
import Import.Text as ImportText
import Json.Decode as Json
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Page.Doc exposing (Msg(..))
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Page.Doc.Theme exposing (applyTheme)
import Page.Empty
import RandomId
import Route
import Session exposing (Session)
import Svg.Attributes
import Task
import Time
import Translation exposing (Language, TranslationId, langToString)
import Types exposing (HeaderMenuState(..), SidebarMenuState(..), SidebarState(..), SortBy(..), TooltipPosition, Tree, ViewMode(..))
import Upgrade exposing (Msg(..))



-- MODEL


type alias Model =
    { loading : Bool
    , documentState : DocumentState
    , sidebarState : SidebarState
    , sidebarMenuState : SidebarMenuState
    , modalState : ModalState
    , fileSearchField : String -- TODO: not needed if switcher isn't open
    , tooltip : Maybe ( Element, TooltipPosition, TranslationId )
    }


type DocumentState
    = Empty Session
    | Doc Page.Doc.Model


type alias DbData =
    { dbName : String, isNew : Bool }


type ModalState
    = NoModal
    | FileSwitcher Doc.Switcher.Model
    | SidebarContextMenu String ( Float, Float )
    | TemplateSelector
    | HelpScreen
    | VideoViewer VideoViewer.Model
    | Wordcount Page.Doc.Model
    | ImportModal ImportModal.Model
    | ImportTextModal ImportText.Model
    | ContactForm ContactForm.Model
    | UpgradeModal


defaultModel : Session -> Maybe Page.Doc.Model -> Model
defaultModel session docModel_ =
    { loading = True
    , documentState =
        case docModel_ of
            Just docModel ->
                Doc docModel

            Nothing ->
                Empty session
    , sidebarState =
        if Session.fileMenuOpen session then
            File

        else
            SidebarClosed
    , sidebarMenuState = NoSidebarMenu
    , modalState = NoModal
    , fileSearchField = ""
    , tooltip = Nothing
    }


init : Session -> Maybe DbData -> ( Model, Cmd Msg )
init session dbData_ =
    case dbData_ of
        Just dbData ->
            if dbData.isNew then
                ( defaultModel session (Just (Page.Doc.init True session dbData.dbName))
                , Cmd.batch
                    [ send <| InitDocument dbData.dbName
                    , Task.attempt (always NoOp) (Browser.Dom.focus "card-edit-1")
                    ]
                )

            else
                ( defaultModel session (Just (Page.Doc.init False session dbData.dbName))
                , send <| LoadDocument dbData.dbName
                )

        Nothing ->
            case Session.lastDocId session of
                Just docId ->
                    ( defaultModel session Nothing, Route.replaceUrl (Session.navKey session) (Route.DocUntitled docId) )

                Nothing ->
                    ( defaultModel session Nothing, send <| GetDocumentList )


isDirty : Model -> Bool
isDirty model =
    case model.documentState of
        Doc docModel ->
            docModel.dirty

        Empty _ ->
            False


getTitle : Model -> Maybe String
getTitle model =
    case model.documentState of
        Doc docModel ->
            Session.getDocName docModel.session docModel.docId

        Empty _ ->
            Nothing


toSession : Model -> Session
toSession { documentState } =
    case documentState of
        Doc docModel ->
            Page.Doc.toUser docModel

        Empty session ->
            session


updateSession : Session -> Model -> Model
updateSession newSession ({ documentState } as model) =
    case documentState of
        Doc docModel ->
            { model | documentState = Doc { docModel | session = newSession } }

        Empty _ ->
            { model | documentState = Empty newSession }



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg
    | LoginStateChanged Session
    | TimeUpdate Time.Posix
    | SettingsChanged Json.Value
    | LogoutRequested
    | Incoming Incoming.Msg
    | LogErr String
      -- Sidebar
    | TemplateSelectorOpened
    | SortByChanged SortBy
    | SidebarStateChanged SidebarState
    | SidebarContextClicked String ( Float, Float )
    | DuplicateDoc String
    | DeleteDoc String
    | ReceivedDocuments DocList.Model
    | SwitcherOpened
    | SwitcherClosed
      -- HELP Modal
    | ToggledHelpMenu Bool
    | ClickedShowVideos
    | VideoViewerOpened
    | VideoViewerMsg VideoViewer.Msg
    | ClickedShowWidget
    | ClickedEmailSupport
    | ContactFormMsg ContactForm.Model ContactForm.Msg
    | CopyEmailClicked Bool
    | ContactFormSubmitted ContactForm.Model
    | ContactFormSent (Result Http.Error ())
      -- Account menu
    | ToggledAccountMenu Bool
    | LanguageMenuRequested (Maybe String)
    | LanguageMenuReceived Element
    | LanguageChanged Language
      -- Import
    | ImportBulkClicked
    | ImportBulkCompleted
    | ImportTextClicked
    | ImportModalMsg ImportModal.Msg
    | ImportTextModalMsg ImportText.Msg
    | ImportTextLoaded ImportText.Settings (List String) (List String)
    | ImportTextIdGenerated Tree (Maybe String) String
    | ImportOpmlRequested
    | ImportOpmlSelected File
    | ImportOpmlLoaded String String
    | ImportOpmlIdGenerated Tree String String
    | ImportOpmlCompleted String
    | ImportJSONRequested
    | ImportJSONSelected File
    | ImportJSONLoaded String String
    | ImportJSONIdGenerated Tree String String
    | ImportJSONCompleted String
      -- Misc UI
    | ToggledUpgradeModal Bool
    | UpgradeModalMsg Upgrade.Msg
    | WordcountModalOpened Page.Doc.Model
    | FileSearchChanged String
    | TooltipRequested String TooltipPosition TranslationId
    | TooltipReceived Element TooltipPosition TranslationId
    | TooltipClosed
    | FullscreenRequested
    | EmptyMessage
    | ModalClosed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotDocMsg docMsg ->
            case model.documentState of
                Doc docModel ->
                    let
                        ( newDocModel, newCmd ) =
                            Page.Doc.update docMsg docModel
                                |> Tuple.mapSecond (Cmd.map GotDocMsg)
                    in
                    case docMsg of
                        -- TODO: Removing tooltips is the only reason Doc.Msgs is fully exposed
                        ShortcutTrayToggle ->
                            ( { model | documentState = Doc newDocModel, tooltip = Nothing }, newCmd )

                        DocSettingsToggled _ ->
                            ( { model | documentState = Doc newDocModel, tooltip = Nothing }, Cmd.none )

                        HistoryToggled _ ->
                            ( { model | documentState = Doc newDocModel, tooltip = Nothing }, Cmd.none )

                        ExportPreviewToggled _ ->
                            ( { model | documentState = Doc newDocModel, tooltip = Nothing }, Cmd.none )

                        _ ->
                            ( { model | documentState = Doc newDocModel }, newCmd )

                Empty _ ->
                    ( model, Cmd.none )

        LoginStateChanged newSession ->
            ( model |> updateSession newSession, Route.pushUrl (Session.navKey newSession) Route.Login )

        TimeUpdate time ->
            ( model |> updateSession (Session.updateTime time session)
            , Cmd.none
            )

        SettingsChanged json ->
            ( model |> updateSession (Session.sync json session), Cmd.none )

        LogoutRequested ->
            ( model, Session.logout )

        Incoming incomingMsg ->
            let
                doNothing =
                    ( model, Cmd.none )

                passThroughTo docModel =
                    Page.Doc.incoming incomingMsg docModel
                        |> (\( d, c ) ->
                                ( { model | documentState = Doc d }, Cmd.map GotDocMsg c )
                           )
            in
            case ( incomingMsg, model.documentState ) of
                ( DataReceived _, Empty _ ) ->
                    ( model, Cmd.none )

                ( Keyboard shortcut, Doc docModel ) ->
                    case model.modalState of
                        FileSwitcher switcherModel ->
                            case shortcut of
                                "enter" ->
                                    case switcherModel.selectedDocument of
                                        Just docId ->
                                            ( model, Route.pushUrl (Session.navKey session) (Route.DocUntitled docId) )

                                        Nothing ->
                                            ( model, Cmd.none )

                                "down" ->
                                    ( { model | modalState = FileSwitcher (Doc.Switcher.down switcherModel) }, Cmd.none )

                                "up" ->
                                    ( { model | modalState = FileSwitcher (Doc.Switcher.up switcherModel) }, Cmd.none )

                                "mod+o" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                "esc" ->
                                    ( { model | fileSearchField = "", modalState = NoModal }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        Wordcount _ ->
                            case shortcut of
                                "w" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                "mod+o" ->
                                    normalMode docModel
                                        (model |> openSwitcher docModel)
                                        (passThroughTo docModel)

                                "esc" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        HelpScreen ->
                            case shortcut of
                                "?" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                "mod+o" ->
                                    normalMode docModel
                                        (model |> openSwitcher docModel)
                                        (passThroughTo docModel)

                                "esc" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )

                        NoModal ->
                            case shortcut of
                                "w" ->
                                    normalMode docModel
                                        ( { model | modalState = Wordcount docModel }, Cmd.none )
                                        (passThroughTo docModel)

                                "?" ->
                                    normalMode docModel
                                        ( { model | modalState = HelpScreen }, Cmd.none )
                                        (passThroughTo docModel)

                                "mod+o" ->
                                    normalMode docModel
                                        (model |> openSwitcher docModel)
                                        (passThroughTo docModel)

                                _ ->
                                    passThroughTo docModel

                        _ ->
                            case shortcut of
                                "esc" ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                "mod+o" ->
                                    normalMode docModel
                                        (model |> openSwitcher docModel)
                                        (passThroughTo docModel)

                                _ ->
                                    passThroughTo docModel

                ( TestTextImportLoaded files, _ ) ->
                    case model.modalState of
                        ImportTextModal modalState ->
                            ( { model | modalState = ImportText.setFileList files modalState |> ImportTextModal }
                            , Cmd.none
                            )

                        _ ->
                            doNothing

                ( _, Doc docModel ) ->
                    passThroughTo docModel

                _ ->
                    doNothing

        LogErr err ->
            ( model
            , send (ConsoleLogRequested err)
            )

        -- Sidebar
        TemplateSelectorOpened ->
            ( { model | modalState = TemplateSelector }, Cmd.none )

        SortByChanged newSort ->
            let
                newSession =
                    Session.setSortBy newSort session
            in
            ( model |> updateSession newSession, send <| SaveUserSetting ( "sortBy", sortByEncoder newSort ) )

        SidebarStateChanged newSidebarState ->
            let
                ( newSessionData, maybeSaveSidebarState ) =
                    case newSidebarState of
                        File ->
                            ( Session.setFileOpen True session, send <| SetSidebarState True )

                        _ ->
                            ( Session.setFileOpen True session, send <| SetSidebarState False )

                newDropdownState =
                    case ( newSidebarState, model.sidebarMenuState ) of
                        ( File, Account _ ) ->
                            NoSidebarMenu

                        ( _, _ ) ->
                            model.sidebarMenuState
            in
            ( { model
                | sidebarState = newSidebarState
                , tooltip = Nothing
                , sidebarMenuState = newDropdownState
              }
                |> updateSession newSessionData
            , maybeSaveSidebarState
            )

        SidebarContextClicked docId ( x, y ) ->
            ( { model | modalState = SidebarContextMenu docId ( x, y ) }, Cmd.none )

        DuplicateDoc docId ->
            ( { model | modalState = NoModal }, Route.replaceUrl (Session.navKey session) (Route.Copy docId) )

        DeleteDoc docId ->
            ( { model | modalState = NoModal }, send <| RequestDelete docId )

        ReceivedDocuments newListState ->
            let
                newSession =
                    Session.updateDocuments newListState session

                ( routeCmd, isLoading ) =
                    case ( model.documentState, Session.documents newSession ) of
                        ( Doc docModel, Success docList ) ->
                            ( docList
                                |> List.map (\d -> Metadata.getDocId d == docModel.docId)
                                |> List.any identity
                                |> (\docStillExists ->
                                        if docStillExists then
                                            Cmd.none

                                        else
                                            Route.replaceUrl (Session.navKey session) Route.Root
                                   )
                            , True
                            )

                        ( Empty _, Success [] ) ->
                            ( Cmd.none, False )

                        ( Empty _, Success docList ) ->
                            ( DocList.getLastUpdated (Success docList)
                                |> Maybe.map (\s -> Route.replaceUrl (Session.navKey session) (Route.DocUntitled s))
                                |> Maybe.withDefault Cmd.none
                            , True
                            )

                        _ ->
                            ( Cmd.none, True )
            in
            ( { model | loading = isLoading } |> updateSession newSession, routeCmd )

        SwitcherOpened ->
            case model.documentState of
                Doc docModel ->
                    openSwitcher docModel model

                Empty _ ->
                    ( model, Cmd.none )

        SwitcherClosed ->
            closeSwitcher model

        -- HELP Modal
        ToggledHelpMenu isOpen ->
            ( { model | modalState = HelpScreen }, Cmd.none )

        ClickedShowVideos ->
            ( { model | modalState = VideoViewer VideoViewer.init, sidebarMenuState = NoSidebarMenu }, Cmd.none )

        VideoViewerOpened ->
            ( { model | modalState = VideoViewer VideoViewer.init }, Cmd.none )

        VideoViewerMsg videoViewerMsg ->
            ( { model | modalState = VideoViewer (VideoViewer.update videoViewerMsg) }, Cmd.none )

        ClickedShowWidget ->
            ( { model | modalState = NoModal }, send <| ShowWidget )

        ClickedEmailSupport ->
            let
                fromEmail =
                    Session.name session
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

        -- Account menu TODO
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

        LanguageChanged newLang ->
            if newLang /= Session.language session then
                ( { model
                    | sidebarMenuState = NoSidebarMenu
                  }
                    |> updateSession (Session.setLanguage newLang session)
                , send <| SaveUserSetting ( "language", langToString newLang |> Enc.string )
                )

            else
                ( model, Cmd.none )

        -- Import
        ImportBulkClicked ->
            ( { model | modalState = ImportModal (ImportModal.init session) }, Cmd.none )

        ImportBulkCompleted ->
            ( { model | modalState = NoModal }, Cmd.none )

        ImportTextClicked ->
            ( { model | modalState = ImportTextModal ImportText.init }, Cmd.none )

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

        ImportTextModalMsg modalMsg ->
            case model.modalState of
                ImportTextModal modalModel ->
                    let
                        u =
                            ImportText.update modalMsg modalModel

                        newCmd =
                            Cmd.batch
                                ([ Cmd.map ImportTextModalMsg u.cmd ]
                                    ++ (if u.sendTestHack then
                                            [ send <| IntegrationTestEvent "ImportTextRequested" ]

                                        else
                                            []
                                       )
                                    ++ (case u.importRequested of
                                            Just ( files, importSettings ) ->
                                                let
                                                    tasks =
                                                        files |> List.map File.toString |> Task.sequence

                                                    metadata =
                                                        files |> List.map File.name
                                                in
                                                [ Task.perform (ImportTextLoaded importSettings metadata) tasks ]

                                            Nothing ->
                                                []
                                       )
                                )
                    in
                    ( { model | modalState = ImportTextModal u.model }, newCmd )

                _ ->
                    ( model, Cmd.none )

        ImportTextLoaded settings metadata markdownStrings ->
            let
                ( importedTree, newSeed, newTitle_ ) =
                    ImportText.toTree (Session.seed session) metadata markdownStrings settings

                newSession =
                    Session.setSeed newSeed session
            in
            ( { model | loading = True } |> updateSession newSession
            , RandomId.generate (ImportTextIdGenerated importedTree newTitle_)
            )

        ImportTextIdGenerated tree newTitle_ docId ->
            let
                author =
                    session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

                encodeMaybeRename =
                    newTitle_
                        |> Maybe.map (\title -> Metadata.renameAndEncode title)
                        |> Maybe.withDefault Metadata.encode

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> encodeMaybeRename)
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
                    Import.Opml.treeResult (Session.seed session) opmlString

                newSession =
                    Session.setSeed newSeed session
            in
            case importTreeResult of
                Ok tree ->
                    ( { model | loading = True } |> updateSession newSession
                    , RandomId.generate (ImportOpmlIdGenerated tree fileName)
                    )

                Err err ->
                    ( model |> updateSession newSession, Cmd.none )

        ImportOpmlIdGenerated tree fileName docId ->
            let
                author =
                    session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> Metadata.renameAndEncode fileName)
            in
            case commitReq_ of
                Just commitReq ->
                    ( model, send <| SaveImportedData commitReq )

                Nothing ->
                    ( model, Cmd.none )

        ImportOpmlCompleted docId ->
            ( model, Route.pushUrl (Session.navKey session) (Route.DocUntitled docId) )

        ImportJSONRequested ->
            ( model, Select.file [ "application/json", "text/plain" ] ImportJSONSelected )

        ImportJSONSelected file ->
            ( model, Task.perform (ImportJSONLoaded (File.name file)) (File.toString file) )

        ImportJSONLoaded fileName jsonString ->
            let
                ( importTreeDecoder, newSeed ) =
                    Import.Single.decoder (Session.seed session)

                newSession =
                    Session.setSeed newSeed session
            in
            case Json.decodeString importTreeDecoder jsonString of
                Ok tree ->
                    ( { model | loading = True } |> updateSession newSession
                    , RandomId.generate (ImportJSONIdGenerated tree fileName)
                    )

                Err err ->
                    ( model |> updateSession newSession, Cmd.none )

        ImportJSONIdGenerated tree fileName docId ->
            let
                author =
                    session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

                commitReq_ =
                    Data.requestCommit tree author Data.empty (Metadata.new docId |> Metadata.renameAndEncode fileName)
            in
            case commitReq_ of
                Just commitReq ->
                    ( model, send <| SaveImportedData commitReq )

                Nothing ->
                    ( model, Cmd.none )

        ImportJSONCompleted docId ->
            ( model, Route.pushUrl (Session.navKey session) (Route.DocUntitled docId) )

        -- Misc UI
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
                    case Session.name session of
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
                            Session.updateUpgrade upgradeModalMsg session

                        maybeFlash =
                            case upgradeModalMsg of
                                PlanChanged _ ->
                                    send <| FlashPrice

                                _ ->
                                    Cmd.none
                    in
                    ( model |> updateSession newSession, maybeFlash )

        WordcountModalOpened docModel ->
            ( { model
                | documentState = Doc { docModel | headerMenu = NoHeaderMenu }
                , modalState = Wordcount docModel
              }
            , Cmd.none
            )

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
            ( model, Cmd.none )

        EmptyMessage ->
            ( model, send <| EmptyMessageShown )

        ModalClosed ->
            case model.modalState of
                VideoViewer _ ->
                    ( { model | modalState = HelpScreen }, Cmd.none )

                ContactForm _ ->
                    ( { model | modalState = HelpScreen }, Cmd.none )

                _ ->
                    ( { model | modalState = NoModal }, Cmd.none )


normalMode : Page.Doc.Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
normalMode docModel modified noOp =
    if docModel.viewState.viewMode == Normal then
        modified

    else
        noOp


openSwitcher : Page.Doc.Model -> Model -> ( Model, Cmd Msg )
openSwitcher docModel model =
    let
        metadata_ =
            Session.getMetadata docModel.session docModel.docId
    in
    case metadata_ of
        Just currentMetadata ->
            ( { model
                | modalState =
                    FileSwitcher
                        { currentDocument = currentMetadata
                        , selectedDocument = Just docModel.docId
                        , searchField = ""
                        , docList = Session.documents (toSession model)
                        }
              }
            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "switcher-input")
            )

        Nothing ->
            ( model, Cmd.none )


closeSwitcher : Model -> ( Model, Cmd Msg )
closeSwitcher model =
    ( { model | modalState = NoModal }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ documentState } as model) =
    let
        session =
            toSession model

        viewTooltip =
            case model.tooltip of
                Just tooltip ->
                    UI.viewTooltip (Session.language session) tooltip

                Nothing ->
                    text ""

        sidebarMsgs =
            { sidebarStateChanged = SidebarStateChanged
            , noOp = NoOp
            , clickedNew = TemplateSelectorOpened
            , tooltipRequested = TooltipRequested
            , tooltipClosed = TooltipClosed
            , clickedSwitcher = SwitcherOpened
            , clickedHelp = ToggledHelpMenu True
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
    in
    case documentState of
        Doc doc ->
            div [ id "app-root", classList [ ( "loading", model.loading ) ], applyTheme doc.theme ]
                (Page.Doc.view
                    { docMsg = GotDocMsg
                    , keyboard = \s -> Incoming (Keyboard s)
                    , tooltipRequested = TooltipRequested
                    , tooltipClosed = TooltipClosed
                    , toggleWordcount = WordcountModalOpened
                    , toggleUpgradeModal = ToggledUpgradeModal
                    }
                    doc
                    ++ [ UI.viewSidebar session
                            sidebarMsgs
                            doc.docId
                            ModifiedAt
                            model.fileSearchField
                            (Session.documents session)
                            (Session.name session |> Maybe.withDefault "" {- TODO -})
                            Nothing
                            model.sidebarMenuState
                            model.sidebarState
                       , viewTooltip
                       ]
                    ++ viewModal session model.modalState
                )

        Empty _ ->
            if model.loading then
                UI.viewAppLoadingSpinner (Session.fileMenuOpen session)

            else
                div [ id "app-root", classList [ ( "loading", model.loading ) ] ]
                    (Page.Empty.view { newClicked = TemplateSelectorOpened, emptyMessage = EmptyMessage }
                        ++ [ UI.viewSidebar session
                                sidebarMsgs
                                ""
                                ModifiedAt
                                ""
                                (Session.documents session)
                                (Session.name session |> Maybe.withDefault "" {- TODO -})
                                Nothing
                                model.sidebarMenuState
                                model.sidebarState
                           , viewTooltip
                           ]
                        ++ viewModal session model.modalState
                    )


viewModal : Session -> ModalState -> List (Html Msg)
viewModal session modalState =
    let
        language =
            Session.language session
    in
    case modalState of
        NoModal ->
            [ text "" ]

        FileSwitcher switcherModel ->
            Doc.Switcher.view SwitcherClosed FileSearchChanged switcherModel

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
                , importTextClicked = ImportTextClicked
                , importOpmlRequested = ImportOpmlRequested
                , importJSONRequested = ImportJSONRequested
                }

        HelpScreen ->
            HelpScreen.view language
                (Session.isMac session)
                { closeModal = ModalClosed
                , showVideoTutorials = VideoViewerOpened
                , showWidget = ClickedShowWidget
                , contactSupport = ClickedEmailSupport
                }

        VideoViewer videoViewerState ->
            VideoViewer.view language ModalClosed VideoViewerMsg videoViewerState

        Wordcount docModel ->
            UI.viewWordCount docModel { modalClosed = ModalClosed }

        ImportModal modalModel ->
            ImportModal.view language modalModel
                |> List.map (Html.map ImportModalMsg)

        ImportTextModal modalModel ->
            ImportText.view
                { closeMsg = TemplateSelectorOpened, tagger = ImportTextModalMsg }
                modalModel

        ContactForm contactFormModel ->
            ContactForm.view language
                { closeMsg = ModalClosed
                , submitMsg = ContactFormSubmitted
                , tagger = ContactFormMsg contactFormModel
                , copyEmail = CopyEmailClicked
                }
                contactFormModel

        UpgradeModal ->
            case Session.upgradeModel session of
                Just upgradeModel ->
                    let
                        daysLeft_ =
                            Session.daysLeft session
                    in
                    Upgrade.view daysLeft_ upgradeModel
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
        , Session.userSettingsChange SettingsChanged
        , Session.loginChanges LoginStateChanged (Session.navKey (toSession model))
        , case model.modalState of
            ImportModal importModalModel ->
                ImportModal.subscriptions importModalModel
                    |> Sub.map ImportModalMsg

            _ ->
                Sub.none
        , Time.every (9 * 1000) TimeUpdate
        ]
