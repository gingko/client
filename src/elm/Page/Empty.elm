module Page.Empty exposing (..)

import Doc.List as DocList
import Doc.UI as UI
import Html exposing (Html, a, br, button, div, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Import.Bulk.UI as ImportModal
import Route
import User exposing (User, language)


type alias Model =
    { user : User
    , documents : DocList.Model
    , modalState : ModalState
    }


type ModalState
    = Closed
    | TemplateSelector
    | ImportModal ImportModal.Model


defaultModel : User -> Model
defaultModel user =
    { user = user
    , documents = DocList.init
    , modalState = Closed
    }


init : User -> ( Model, Cmd msg )
init user =
    case User.lastDocId user of
        Just docId ->
            ( defaultModel user, Route.replaceUrl (User.navKey user) (Route.DocUntitled docId) )

        Nothing ->
            ( defaultModel user, DocList.fetch user )


toUser : Model -> User
toUser model =
    model.user



-- UPDATE


type Msg
    = NoOp
    | NewClicked
    | ModalClosed
    | ImportModalMsg ImportModal.Msg
    | ImportBulkClicked
    | ImportJSONRequested
    | ReceivedDocuments DocList.Model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ReceivedDocuments docList ->
            case DocList.getLastUpdated docList of
                Nothing ->
                    ( { model | documents = docList }, Cmd.none )

                Just docId ->
                    ( model, Route.replaceUrl (User.navKey model.user) (Route.DocUntitled docId) )

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
            ( { model | modalState = ImportModal <| ImportModal.init model.user }, Cmd.none )

        ImportJSONRequested ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view ({ user, documents } as model) =
    div
        [ id "app-root", class "loading" ]
        ([ UI.viewHomeLink False
         , div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , if DocList.isLoading documents then
            div [ id "empty-message" ]
                [ text "Loading..." ]

           else
            div [ id "empty-message" ]
                [ text "You don't have any documents. Create one here:"
                , br [] []
                , button [ id "new-button", onClick NewClicked ] [ text "New" ]
                ]
         ]
            ++ UI.viewSidebarStatic False
            ++ viewModal model
        )


viewModal : Model -> List (Html Msg)
viewModal ({ user } as model) =
    case model.modalState of
        Closed ->
            []

        TemplateSelector ->
            UI.viewTemplateSelector (User.language user)
                { modalClosed = ModalClosed
                , importBulkClicked = ImportBulkClicked
                , importJSONRequested = ImportJSONRequested
                }

        ImportModal importModalState ->
            ImportModal.view (User.language user) importModalState |> List.map (Html.map ImportModalMsg)



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
        ]
