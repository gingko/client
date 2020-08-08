module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Decode exposing (Value)
import Page.Doc
import Page.Home
import Page.Login
import Session exposing (Session)
import Url exposing (Url)



-- MODEL


type Model
    = Redirect Session
    | Login Page.Login.Model
    | Home Page.Home.Model
    | Doc Page.Doc.Model


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeEmail url navKey =
    changeRouteTo url (Redirect (Session.fromData navKey maybeEmail))


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    let
        session =
            toSession model
    in
    if Session.loggedIn session then
        case url.path of
            "/" ->
                Page.Home.init session
                    |> updateWith Home GotHomeMsg

            "/login" ->
                Page.Login.init session
                    |> updateWith Login GotLoginMsg

            dbNamePath ->
                Page.Doc.init session (String.dropLeft 1 dbNamePath)
                    |> updateWith Doc GotDocMsg

    else
        let
            ( loginModel, loginCmds ) =
                Page.Login.init session
                    |> updateWith Login GotLoginMsg
        in
        case url.path of
            "/login" ->
                ( loginModel, loginCmds )

            _ ->
                ( loginModel, Cmd.batch [ loginCmds, Nav.replaceUrl (Session.navKey session) "/login" ] )


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        Login login ->
            Page.Login.toSession login

        Home home ->
            Page.Home.toSession home

        Doc doc ->
            Page.Doc.toSession doc



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Redirect _ ->
            { title = "Loading...", body = [ Html.div [] [ Html.text "LOADING..." ] ] }

        Login login ->
            { title = "Gingko - Login", body = [ Html.map GotLoginMsg (Page.Login.view login) ] }

        Home home ->
            { title = "Gingko - Home", body = [ Html.map GotHomeMsg (Page.Home.view home) ] }

        Doc doc ->
            { title = "Gingko", body = [ Html.map GotDocMsg (Page.Doc.view doc) ] }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotLoginMsg Page.Login.Msg
    | GotHomeMsg Page.Home.Msg
    | GotDocMsg Page.Doc.Msg
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            let
                _ =
                    Debug.log "Main ChangedUrl" url
            in
            changeRouteTo url model

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            Page.Login.update loginMsg loginModel
                |> updateWith Login GotLoginMsg

        ( GotDocMsg docMsg, Doc docModel ) ->
            Page.Doc.update docMsg docModel
                |> updateWith Doc GotDocMsg

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        ( GotSession session, Redirect _ ) ->
            -- TODO: Does this serve any purpose?
            ( Redirect session, Nav.replaceUrl (Session.navKey session) "/" )

        _ ->
            let
                _ =
                    Debug.log "(msg, model)" ( msg, model )
            in
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

        Login pageModel ->
            Sub.map GotLoginMsg (Page.Login.subscriptions pageModel)

        Home _ ->
            Sub.none

        Doc pageModel ->
            Sub.map GotDocMsg (Page.Doc.subscriptions pageModel)



-- MAIN


main : Program (Maybe String) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
