module Page.Empty exposing (..)

import Doc.List as DocList
import Doc.UI as UI
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, id)
import Route
import User exposing (User)


type alias Model =
    { user : User, documents : DocList.Model }


init : User -> ( Model, Cmd msg )
init user =
    case User.lastDocId user of
        Just docId ->
            ( { user = user, documents = DocList.init }, Route.replaceUrl (User.navKey user) (Route.DocUntitled docId) )

        Nothing ->
            ( { user = user, documents = DocList.init }, DocList.fetch user )


toUser : Model -> User
toUser model =
    model.user



-- UPDATE


type Msg
    = NoOp
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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "app-root", class "loading" ]
        ([ UI.viewHomeLink False
         , div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , if DocList.isLoading model.documents then
            div [ id "empty-message" ]
                [ text "Loading..." ]

           else
            div [ id "empty-message" ]
                [ text "You don't have any documents. Create one here:"
                , a [ href <| Route.toString Route.DocNew ] [ text "NEW" ]
                ]
         ]
            ++ UI.viewSidebarStatic False
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    DocList.subscribe ReceivedDocuments
