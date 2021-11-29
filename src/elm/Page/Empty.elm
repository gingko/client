module Page.Empty exposing (..)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Doc.ContactForm as ContactForm
import Doc.Data as Data
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata
import Doc.UI as UI
import Doc.VideoViewer as VideoViewer
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, br, button, div, h1, img, p, text)
import Html.Attributes exposing (class, href, id, src)
import Html.Events exposing (on, onClick)
import Http
import Import.Bulk.UI as ImportModal
import Import.Incoming
import Import.Text
import Json.Decode as Dec
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import RandomId
import Route
import Session exposing (Session)
import Task
import Translation exposing (Language, langToString)
import Types exposing (SidebarMenuState(..), SidebarState(..), SortBy(..), TooltipPosition, Tree)


type alias Model =
    { session : Session
    , modalState : ModalState
    , sidebarState : SidebarState
    , sidebarMenuState : SidebarMenuState
    , tooltip : Maybe ( Element, TooltipPosition, String )
    }


type ModalState
    = Closed
    | TemplateSelector
    | VideoViewer VideoViewer.Model
    | ImportModal ImportModal.Model
    | ContactForm ContactForm.Model


defaultModel : Session -> Model
defaultModel user =
    { session = user
    , modalState = Closed
    , sidebarState =
        if Session.fileMenuOpen user then
            File

        else
            SidebarClosed
    , sidebarMenuState = NoSidebarMenu
    , tooltip = Nothing
    }


init : Session -> ( Model, Cmd msg )
init session =
    case Session.lastDocId session of
        Just docId ->
            ( defaultModel session, Route.replaceUrl (Session.navKey session) (Route.DocUntitled docId) )

        Nothing ->
            ( defaultModel session, send <| GetDocumentList )


toUser : Model -> Session
toUser model =
    model.session



-- UPDATE


type Msg
    = NoOp
    | EmptyMessage
    | NewClicked
    | ModalClosed
    | SidebarStateChanged SidebarState
    | ToggledHelpMenu Bool
    | ClickedEmailSupport
    | ClickedShowVideos
    | VideoViewerMsg VideoViewer.Msg
    | ContactFormMsg ContactForm.Model ContactForm.Msg
    | CopyEmailClicked Bool
    | ContactFormSubmitted ContactForm.Model
    | ContactFormSent (Result Http.Error ())
    | ToggledAccountMenu Bool
    | LanguageMenuRequested (Maybe String)
    | LanguageMenuReceived Element
    | LanguageChanged Language
    | LogoutRequested
    | ImportModalMsg ImportModal.Msg
    | ImportBulkClicked
    | ImportBulkCompleted
    | ImportMarkdownRequested
    | ImportMarkdownSelected File (List File)
    | ImportMarkdownLoaded (List String) (List String)
    | ImportMarkdownIdGenerated Tree String
    | ImportJSONRequested
    | ImportJSONCompleted String
    | ReceivedDocuments DocList.Model
    | TooltipRequested String TooltipPosition String
    | TooltipReceived Element TooltipPosition String
    | TooltipClosed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedDocuments newList ->
            let
                updatedSession =
                    Session.updateDocuments newList model.session

                routeCmd =
                    case Session.documents updatedSession of
                        Success [] ->
                            Cmd.none

                        Success docList ->
                            DocList.getLastUpdated (Success docList)
                                |> Maybe.map (\s -> Route.replaceUrl (Session.navKey model.session) (Route.DocUntitled s))
                                |> Maybe.withDefault Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model | session = updatedSession }, routeCmd )

        EmptyMessage ->
            ( model, send <| EmptyMessageShown )

        NewClicked ->
            ( { model | modalState = TemplateSelector }, Cmd.none )

        ModalClosed ->
            ( { model | modalState = Closed }, Cmd.none )

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

        ToggledHelpMenu isOpen ->
            let
                ( newDropdownState, newSidebarState ) =
                    if isOpen then
                        ( Help, SidebarClosed )

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

        ClickedShowVideos ->
            ( { model | modalState = VideoViewer VideoViewer.init }, Cmd.none )

        VideoViewerMsg videoViewerMsg ->
            ( { model | modalState = VideoViewer (VideoViewer.update videoViewerMsg) }, Cmd.none )

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
                    ( { model | modalState = Closed }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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
            if newLang /= Session.language model.session then
                ( { model
                    | session = Session.setLanguage newLang model.session
                    , sidebarMenuState = NoSidebarMenu
                  }
                , send <| SaveUserSetting ( "language", langToString newLang |> Enc.string )
                )

            else
                ( model, Cmd.none )

        LogoutRequested ->
            ( model, Session.logout )

        ImportModalMsg importModalMsg ->
            case model.modalState of
                ImportModal importModalState ->
                    let
                        ( newModalState, newCmd ) =
                            ImportModal.update importModalMsg importModalState
                                |> Tuple.mapSecond (Cmd.map ImportModalMsg)
                    in
                    ( { model | modalState = ImportModal newModalState }, newCmd )

                _ ->
                    ( model, Cmd.none )

        ImportBulkClicked ->
            ( { model | modalState = ImportModal <| ImportModal.init model.session }, Cmd.none )

        ImportBulkCompleted ->
            ( { model | modalState = Closed }, Cmd.none )

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
                    Import.Text.toTree (Session.seed model.session) metadata markdownStrings

                newSession =
                    Session.setSeed newSeed model.session
            in
            ( { model | session = newSession }, RandomId.generate (ImportMarkdownIdGenerated importedTree) )

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

        ImportJSONRequested ->
            ( model, Cmd.none )

        ImportJSONCompleted docId ->
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



-- VIEW


view : Model -> Html Msg
view ({ session } as model) =
    let
        lang =
            Session.language session
    in
    div
        [ id "app-root", class "loading" ]
        ([ div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , case Session.documents session of
            Success [] ->
                div [ id "empty-message" ]
                    [ h1 [] [ text "You don't have any documents" ]
                    , p [] [ text "Click to create one:" ]
                    , br [] []
                    , div [ id "new-button", onClick NewClicked ] [ AntIcons.fileAddOutlined [] ]
                    , img [ src "", on "error" (Dec.succeed EmptyMessage) ] []
                    ]

            _ ->
                div [ id "loading-spinner" ]
                    [ text "Loading..." ]
         , UI.viewSidebar
            session
            { sidebarStateChanged = SidebarStateChanged
            , noOp = NoOp
            , clickedNew = NewClicked
            , tooltipRequested = TooltipRequested
            , tooltipClosed = TooltipClosed
            , clickedSwitcher = NoOp
            , clickedHelp = ToggledHelpMenu (not (model.sidebarMenuState == Help))
            , toggledShortcuts = NoOp
            , clickedEmailSupport = ClickedEmailSupport
            , clickedShowVideos = ClickedShowVideos
            , languageMenuRequested = LanguageMenuRequested
            , logout = LogoutRequested
            , toggledAccount = ToggledAccountMenu
            , fileSearchChanged = always NoOp
            , changeSortBy = always NoOp
            , contextMenuOpened = \_ -> \_ -> NoOp
            , languageChanged = LanguageChanged
            , fullscreenRequested = NoOp
            }
            (Metadata.new "")
            ModifiedAt
            ""
            (Session.documents model.session)
            (Session.name model.session |> Maybe.withDefault "" {- TODO -})
            Nothing
            model.sidebarMenuState
            model.sidebarState
         , model.tooltip |> Maybe.map UI.viewTooltip |> Maybe.withDefault (text "")
         ]
            ++ viewModal model
        )


viewModal : Model -> List (Html Msg)
viewModal ({ session } as model) =
    let
        language =
            Session.language session
    in
    case model.modalState of
        Closed ->
            []

        TemplateSelector ->
            UI.viewTemplateSelector (Session.language session)
                { modalClosed = ModalClosed
                , importBulkClicked = ImportBulkClicked
                , importTextClicked = ImportMarkdownRequested

                {- TODO -}
                , importOpmlRequested = NoOp
                , importJSONRequested = ImportJSONRequested
                }

        VideoViewer viewerState ->
            VideoViewer.view language ModalClosed VideoViewerMsg viewerState

        ImportModal importModalState ->
            ImportModal.view (Session.language session) importModalState |> List.map (Html.map ImportModalMsg)

        ContactForm contactFormModel ->
            ContactForm.view language
                { closeMsg = ModalClosed
                , submitMsg = ContactFormSubmitted
                , tagger = ContactFormMsg contactFormModel
                , copyEmail = CopyEmailClicked
                }
                contactFormModel



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { modalState } =
    Sub.batch
        [ DocList.subscribe ReceivedDocuments
        , case modalState of
            ImportModal importModalState ->
                ImportModal.subscriptions importModalState |> Sub.map ImportModalMsg

            _ ->
                Sub.none
        , Import.Incoming.importComplete
            (\docId_ ->
                case docId_ of
                    Just docId ->
                        ImportJSONCompleted docId

                    Nothing ->
                        ImportBulkCompleted
            )
        ]
