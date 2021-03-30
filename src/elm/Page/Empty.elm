module Page.Empty exposing (..)

import Doc.List as DocList exposing (Model(..))
import Doc.UI as UI
import Html exposing (Html, a, br, button, div, img, text)
import Html.Attributes exposing (class, href, id, src)
import Html.Events exposing (on, onClick)
import Import.Bulk.UI as ImportModal
import Import.Incoming
import Json.Decode as Dec
import Outgoing exposing (Msg(..), send)
import Route
import Session exposing (Session, language)


type alias Model =
    { session : Session
    , modalState : ModalState
    }


type ModalState
    = Closed
    | TemplateSelector
    | ImportModal ImportModal.Model


defaultModel : Session -> Model
defaultModel user =
    { session = user
    , modalState = Closed
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
    | ImportModalMsg ImportModal.Msg
    | ImportBulkClicked
    | ImportBulkCompleted
    | ImportJSONRequested
    | ImportJSONCompleted String
    | ReceivedDocuments DocList.Model


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

        ImportJSONRequested ->
            ( model, Cmd.none )

        ImportJSONCompleted docId ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ session } as model) =
    div
        [ id "app-root", class "loading" ]
        ([ div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , case Session.documents session of
            Success [] ->
                div [ id "empty-message" ]
                    [ text "You don't have any documents. Create one here:"
                    , br [] []
                    , button [ id "new-button", onClick NewClicked ] [ text "New" ]
                    , img [ src "", on "error" (Dec.succeed EmptyMessage) ] []
                    ]

            _ ->
                div [ id "loading-spinner" ]
                    [ text "Loading..." ]
         ]
            ++ UI.viewSidebarStatic False
            ++ viewModal model
        )


viewModal : Model -> List (Html Msg)
viewModal ({ session } as model) =
    case model.modalState of
        Closed ->
            []

        TemplateSelector ->
            UI.viewTemplateSelector (Session.language session)
                { modalClosed = ModalClosed
                , importBulkClicked = ImportBulkClicked
                , importJSONRequested = ImportJSONRequested
                }

        ImportModal importModalState ->
            ImportModal.view (Session.language session) importModalState |> List.map (Html.map ImportModalMsg)



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
