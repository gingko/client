module Page.App exposing (Model, Msg, init, toUser, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Doc.ContactForm as ContactForm
import Doc.HelpScreen as HelpScreen
import Doc.Metadata as Metadata
import Doc.Switcher
import Doc.UI as UI
import Doc.VideoViewer as VideoViewer
import File exposing (File)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, id, style)
import Html.Events exposing (onClick)
import Http
import Import.Bulk.UI as ImportModal
import Import.Text as ImportText
import Outgoing exposing (Msg(..), send)
import Page.Doc
import Page.Empty
import Session exposing (Session)
import Svg.Attributes
import Translation exposing (Language)
import Types exposing (SidebarMenuState(..), SidebarState(..), SortBy(..), TooltipPosition, Tree)
import Upgrade



-- MODEL


type alias Model =
    { session : Session
    , document : Maybe Page.Doc.Model
    , sidebarState : SidebarState
    , sidebarMenuState : SidebarMenuState
    , modalState : ModalState
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
    , document = Nothing
    , sidebarState =
        if Session.fileMenuOpen session then
            File

        else
            SidebarClosed
    , sidebarMenuState = NoSidebarMenu
    , modalState = NoModal
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
    | ModalClosed
    | ImportBulkClicked
    | ImportTextClicked
    | ImportOpmlRequested
    | ImportJSONRequested
    | SidebarStateChanged SidebarState
    | FileSearchChanged String
    | DuplicateDoc String
    | DeleteDoc String
    | VideoViewerOpened
    | VideoViewerMsg VideoViewer.Msg
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TemplateSelectorOpened ->
            ( { model | modalState = TemplateSelector }, Cmd.none )

        _ ->
            ( model, Cmd.none )



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
