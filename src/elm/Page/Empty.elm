module Page.Empty exposing (..)

import Doc.UI as UI
import Html exposing (Html, a, div, text)
import Html.Attributes exposing (class, href, id)
import Route
import User exposing (User)


type alias Model =
    User


init : User -> ( Model, Cmd msg )
init user =
    ( user, Cmd.none )


toUser : Model -> User
toUser model =
    model



-- UPDATE


type Msg
    = NoOp



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ id "app-root", class "loading" ]
        ([ UI.viewHomeLink False
         , div [ id "document-header" ] []
         , div [ id "loading-overlay" ] []
         , div []
            [ text "You don't have any documents. Create one here:"
            , a [ href <| Route.toString Route.DocNew ] [ text "NEW" ]
            ]
         ]
            ++ UI.viewSidebarStatic False
        )
