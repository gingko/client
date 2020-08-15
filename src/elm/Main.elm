module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Decode exposing (Value)
import Page.Doc
import Page.Home
import Page.Login
import Page.NotFound
import Page.Signup
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Signup Page.Signup.Model
    | Login Page.Login.Model
    | Home Page.Home.Model
    | Doc Page.Doc.Model


init : Maybe String -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeEmail url navKey =
    changeRouteTo (Route.fromUrl url) (Redirect (Session.fromData navKey maybeEmail))


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    if Session.loggedIn session then
        case maybeRoute of
            Just Route.Home ->
                Page.Home.init session |> updateWith Home GotHomeMsg

            Just Route.Signup ->
                Page.Signup.init session |> updateWith Signup GotSignupMsg

            Just Route.Login ->
                Page.Login.init session |> updateWith Login GotLoginMsg

            Just (Route.Doc dbName _) ->
                Page.Doc.init session dbName |> updateWith Doc GotDocMsg

            Just (Route.DocUntitled dbName) ->
                Page.Doc.init session dbName |> updateWith Doc GotDocMsg

            Nothing ->
                ( NotFound session, Cmd.none )

    else
        let
            ( signupModel, signupCmds ) =
                Page.Signup.init session
                    |> updateWith Signup GotSignupMsg

            ( loginModel, loginCmds ) =
                Page.Login.init session
                    |> updateWith Login GotLoginMsg
        in
        case maybeRoute of
            Just Route.Signup ->
                ( signupModel, signupCmds )

            Just Route.Login ->
                ( loginModel, loginCmds )

            _ ->
                ( loginModel, Cmd.batch [ loginCmds, Nav.replaceUrl (Session.navKey session) "/login" ] )


toSession : Model -> Session
toSession page =
    case page of
        Redirect session ->
            session

        NotFound session ->
            session

        Signup signup ->
            Page.Signup.toSession signup

        Login login ->
            Page.Login.toSession login

        Home home ->
            Page.Home.toSession home

        Doc doc ->
            Page.Doc.toSession doc



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotSignupMsg Page.Signup.Msg
    | GotLoginMsg Page.Login.Msg
    | GotHomeMsg Page.Home.Msg
    | GotDocMsg Page.Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotSignupMsg signupMsg, Signup signupModel ) ->
            Page.Signup.update signupMsg signupModel
                |> updateWith Signup GotSignupMsg

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            Page.Login.update loginMsg loginModel
                |> updateWith Login GotLoginMsg

        ( GotDocMsg docMsg, Doc docModel ) ->
            Page.Doc.update docMsg docModel
                |> updateWith Doc GotDocMsg

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Redirect _ ->
            { title = "Loading...", body = [ Html.div [] [ Html.text "LOADING..." ] ] }

        NotFound _ ->
            Page.NotFound.view

        Signup signup ->
            { title = "Gingko - Signup", body = [ Html.map GotSignupMsg (Page.Signup.view signup) ] }

        Login login ->
            { title = "Gingko - Login", body = [ Html.map GotLoginMsg (Page.Login.view login) ] }

        Home home ->
            { title = "Gingko - Home", body = [ Html.map GotHomeMsg (Page.Home.view home) ] }

        Doc doc ->
            { title = "Gingko", body = [ Html.map GotDocMsg (Page.Doc.view doc) ] }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Sub.none

        NotFound _ ->
            Sub.none

        Signup pageModel ->
            Sub.map GotSignupMsg (Page.Signup.subscriptions pageModel)

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
