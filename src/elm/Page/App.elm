module Page.App exposing (Model, Msg, init, toUser, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (sortByEncoder)
import Doc.ContactForm as ContactForm
import Doc.Data as Data
import Doc.HelpScreen as HelpScreen
import Doc.List as DocList
import Doc.Metadata as Metadata
import Doc.Switcher
import Doc.UI as UI
import Doc.VideoViewer as VideoViewer
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)
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
import Page.Doc
import Page.Doc.Incoming as Incoming
import Page.Empty
import RandomId
import Route
import Session exposing (Session)
import Svg.Attributes
import Task
import Time
import Translation exposing (Language, langToString)
import Types exposing (SidebarMenuState(..), SidebarState(..), SortBy(..), TooltipPosition, Tree)
import Upgrade exposing (Msg(..))



-- MODEL


type alias Model =
    { session : Session
    , loading : Bool
    , document : Maybe Page.Doc.Model
    , sidebarState : SidebarState
    , sidebarMenuState : SidebarMenuState
    , modalState : ModalState
    , fileSearchField : String -- TODO: not needed if switcher isn't open
    , tooltip : Maybe ( Element, TooltipPosition, String )
    }


type alias DbData =
    { dbName : String, isNew : Bool }


type ModalState
    = NoModal
    | FileSwitcher Doc.Switcher.Model
    | SidebarContextMenu String ( Float, Float )
    | TemplateSelector
    | HelpScreen
    | VideoViewer VideoViewer.Model
    | Wordcount
    | ImportModal ImportModal.Model
    | ImportTextModal ImportText.Model
    | ContactForm ContactForm.Model
    | UpgradeModal


defaultModel : Session -> Model
defaultModel session =
    { session = session
    , loading = True
    , document = Nothing
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


init : Session -> Maybe DbData -> ( Model, Cmd msg )
init session dbData_ =
    case dbData_ of
        Just dbData ->
            if dbData.isNew then
                ( defaultModel session
                , send <| InitDocument dbData.dbName
                )

            else
                ( defaultModel session, send <| LoadDocument dbData.dbName )

        Nothing ->
            ( defaultModel session, Cmd.none )


toUser : Model -> Session
toUser { session } =
    session



-- UPDATE


type Msg
    = NoOp
    | TemplateSelectorOpened
    | SwitcherOpened
    | SwitcherClosed
    | WordcountModalOpened
    | ModalClosed
    | ImportBulkClicked
    | ImportTextClicked
    | ImportOpmlRequested
    | ImportJSONRequested
    | SidebarStateChanged SidebarState
    | FileSearchChanged String
    | TimeUpdate Time.Posix
    | DuplicateDoc String
    | DeleteDoc String
    | VideoViewerOpened
    | VideoViewerMsg VideoViewer.Msg
    | ReceivedDocuments DocList.Model
    | SettingsChanged Json.Value
    | LoginStateChanged Session
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
    | ImportTextModalMsg ImportText.Msg
    | ImportTextLoaded ImportText.Settings (List String) (List String)
    | ImportTextIdGenerated Tree (Maybe String) String
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
    | GotDocMsg Page.Doc.Msg
    | TooltipRequested String TooltipPosition String
    | TooltipReceived Element TooltipPosition String
    | TooltipClosed
    | FullscreenRequested
    | ToggledHelpMenu Bool
    | ShortcutTrayToggle
    | LanguageMenuRequested (Maybe String)
    | LanguageMenuReceived Element
    | ToggledAccountMenu Bool
    | LogoutRequested
    | SidebarContextClicked String ( Float, Float )
    | SortByChanged SortBy
    | Incoming Incoming.Msg
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        LogoutRequested ->
            ( model, Session.logout )

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

        ReceivedDocuments newListState ->
            let
                updatedSession =
                    Session.updateDocuments newListState model.session

                ( newModel, newCmd ) =
                    {--case DocList.current model.metadata newListState of
                        Just currentMetadata ->
                            ( { model | metadata = currentMetadata, titleField = Metadata.getDocName currentMetadata, session = updatedSession }, Cmd.none )

                        Nothing ->
                            ( { model | session = updatedSession }
                            , Route.replaceUrl (Session.navKey model.session) Route.Root
                            )
                            --}
                    ( model, Cmd.none )
            in
            ( newModel, newCmd )

        SettingsChanged json ->
            ( { model | session = Session.sync json model.session }, Cmd.none )

        SwitcherOpened ->
            openSwitcher model

        SwitcherClosed ->
            closeSwitcher model

        WordcountModalOpened ->
            ( { model | modalState = Wordcount }, Cmd.none )

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

        TimeUpdate time ->
            ( { model | session = Session.updateTime time model.session }
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

        ImportTextClicked ->
            ( { model | modalState = ImportTextModal ImportText.init }, Cmd.none )

        ImportTextLoaded settings metadata markdownStrings ->
            let
                ( importedTree, newSeed, newTitle_ ) =
                    ImportText.toTree (Session.seed model.session) metadata markdownStrings settings

                newSession =
                    Session.setSeed newSeed model.session
            in
            ( { model | loading = True, session = newSession }
            , RandomId.generate (ImportTextIdGenerated importedTree newTitle_)
            )

        ImportTextIdGenerated tree newTitle_ docId ->
            let
                author =
                    model.session |> Session.name |> Maybe.withDefault "jane.doe@gmail.com"

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

        ShortcutTrayToggle ->
            let
                newIsOpen =
                    not <| Session.shortcutTrayOpen model.session
            in
            ( { model
                | session = Session.setShortcutTrayOpen newIsOpen model.session

                -- TODO
                , tooltip = Nothing
              }
            , send <| SaveUserSetting ( "shortcutTrayOpen", Enc.bool newIsOpen )
            )

        _ ->
            ( model, Cmd.none )


openSwitcher : Model -> ( Model, Cmd Msg )
openSwitcher model =
    let
        metadata =
            Metadata.new ""

        --TODO
    in
    ( { model
        | modalState =
            FileSwitcher
                { currentDocument = metadata
                , selectedDocument = Just (Metadata.getDocId metadata)
                , searchField = "" --TODO
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
view ({ session, document } as model) =
    case document of
        Just doc ->
            Page.Doc.view doc |> Html.map GotDocMsg

        Nothing ->
            let
                sidebarMsgs =
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
            in
            div [ id "app-root", class "loading" ]
                (Page.Empty.view TemplateSelectorOpened
                    ++ [ UI.viewSidebar session
                            sidebarMsgs
                            (Metadata.new "")
                            ModifiedAt
                            ""
                            (Session.documents session)
                            (Session.name session |> Maybe.withDefault "" {- TODO -})
                            Nothing
                            model.sidebarMenuState
                            model.sidebarState
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
                , importTextClicked = ImportTextClicked
                , importOpmlRequested = ImportOpmlRequested
                , importJSONRequested = ImportJSONRequested
                }

        HelpScreen ->
            HelpScreen.view (Session.isMac session)
                { closeModal = ModalClosed
                , showVideoTutorials = VideoViewerOpened
                , showWidget = ClickedShowWidget
                , contactSupport = ClickedEmailSupport
                }

        VideoViewer videoViewerState ->
            VideoViewer.view language ModalClosed VideoViewerMsg videoViewerState

        Wordcount ->
            --UI.viewWordCount model { modalClosed = ModalClosed }
            []

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
        , Session.loginChanges LoginStateChanged (Session.navKey model.session)
        , case model.modalState of
            ImportModal importModalModel ->
                ImportModal.subscriptions importModalModel
                    |> Sub.map ImportModalMsg

            _ ->
                Sub.none
        , Time.every (9 * 1000) TimeUpdate
        ]
