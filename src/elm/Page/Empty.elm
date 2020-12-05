module Page.Empty exposing (..)

import Doc.List as DocList
import Doc.UI as UI
import Html exposing (Html, a, br, button, div, text)
import Html.Attributes exposing (class, href, id)
import Html.Events exposing (onClick)
import Route
import User exposing (User, language)


type alias Model =
    { user : User
    , documents : DocList.Model
    , selectorOpen : Bool
    }


defaultModel : User -> Model
defaultModel user =
    { user = user
    , documents = DocList.init
    , selectorOpen = False
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
            ( { model | selectorOpen = True }, Cmd.none )

        ModalClosed ->
            ( { model | selectorOpen = False }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { user, documents, selectorOpen } =
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
            ++ (if selectorOpen then
                    UI.viewTemplateSelector (User.language user)
                        { modalClosed = ModalClosed
                        , importBulkClicked = NoOp
                        , importJSONRequested = NoOp
                        }

                else
                    []
               )
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    DocList.subscribe ReceivedDocuments
