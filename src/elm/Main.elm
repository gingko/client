module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Decode exposing (Decoder, Value)
import Page.Doc
import Page.DocNew
import Page.Home
import Page.Login
import Page.NotFound
import Page.ResetPassword
import Page.Signup
import Route exposing (Route)
import Url exposing (Url)
import User exposing (User)



-- MODEL


type Model
    = Redirect User
    | NotFound User
    | Signup Page.Signup.Model
    | Login Page.Login.Model
    | ResetPassword Page.ResetPassword.Model
    | Home Page.Home.Model
    | DocNew User
    | Doc Page.Doc.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        user =
            User.decode navKey json
    in
    case ( User.loggedIn user, Route.fromUrl url ) of
        ( True, _ ) ->
            changeRouteTo (Route.fromUrl url) (Redirect user)

        ( False, Just Route.Login ) ->
            changeRouteTo (Just Route.Login) (Redirect user)

        ( False, Just (Route.ResetPassword token) ) ->
            changeRouteTo (Just (Route.ResetPassword token)) (Redirect user)

        ( False, _ ) ->
            changeRouteTo (Just Route.Signup) (Redirect user)


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        user =
            toUser model
    in
    if User.loggedIn user then
        case maybeRoute of
            Just Route.Home ->
                Page.Home.init user |> updateWith Home GotHomeMsg

            Just Route.Signup ->
                Page.Signup.init user |> updateWith Signup GotSignupMsg

            Just Route.Login ->
                Page.Login.init user |> updateWith Login GotLoginMsg

            Just Route.Logout ->
                let
                    ( loggedOutUser, logoutCmd ) =
                        User.logout user
                in
                Page.Login.init loggedOutUser
                    |> updateWith Login GotLoginMsg
                    |> withCmd logoutCmd

            Just (Route.ResetPassword token) ->
                let
                    ( loggedOutUser, logoutCmd ) =
                        User.logout user
                in
                Page.ResetPassword.init loggedOutUser token
                    |> updateWith ResetPassword GotResetPasswordMsg
                    |> withCmd logoutCmd

            Just Route.DocNew ->
                Page.DocNew.init user |> updateWith DocNew GotDocNewMsg

            Just (Route.DocUntitled dbName) ->
                let
                    isNew =
                        case model of
                            DocNew _ ->
                                True

                            _ ->
                                False
                in
                Page.Doc.init user dbName isNew |> updateWith Doc GotDocMsg

            Just (Route.Doc dbName _) ->
                Page.Doc.init user dbName False |> updateWith Doc GotDocMsg

            Nothing ->
                ( NotFound user, Cmd.none )

    else
        let
            ( signupModel, signupCmds ) =
                Page.Signup.init user
                    |> updateWith Signup GotSignupMsg

            ( loginModel, loginCmds ) =
                Page.Login.init user
                    |> updateWith Login GotLoginMsg
        in
        case maybeRoute of
            Just Route.Signup ->
                ( signupModel, Cmd.batch [ signupCmds, Route.replaceUrl (User.navKey user) Route.Signup ] )

            Just Route.Login ->
                ( loginModel, loginCmds )

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init user token
                    |> updateWith ResetPassword GotResetPasswordMsg

            _ ->
                ( loginModel, Cmd.batch [ loginCmds, Route.replaceUrl (User.navKey user) Route.Login ] )


toUser : Model -> User
toUser page =
    case page of
        Redirect user ->
            user

        NotFound user ->
            user

        Signup signup ->
            Page.Signup.toUser signup

        Login login ->
            Page.Login.toUser login

        ResetPassword signup ->
            Page.ResetPassword.toUser signup

        Home home ->
            Page.Home.toUser home

        DocNew user ->
            user

        Doc doc ->
            Page.Doc.toUser doc



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotSignupMsg Page.Signup.Msg
    | GotLoginMsg Page.Login.Msg
    | GotResetPasswordMsg Page.ResetPassword.Msg
    | GotHomeMsg Page.Home.Msg
    | GotDocNewMsg Page.DocNew.Msg
    | GotDocMsg Page.Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, Signup _ ) ->
            case Route.fromUrl url of
                Just Route.Signup ->
                    ( model, Cmd.none )

                otherRoute ->
                    changeRouteTo otherRoute model

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (User.navKey (toUser model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( GotSignupMsg signupMsg, Signup signupModel ) ->
            Page.Signup.update signupMsg signupModel
                |> updateWith Signup GotSignupMsg

        ( GotLoginMsg loginMsg, Login loginModel ) ->
            Page.Login.update loginMsg loginModel
                |> updateWith Login GotLoginMsg

        ( GotResetPasswordMsg resetPassMsg, ResetPassword resetPassModel ) ->
            Page.ResetPassword.update resetPassMsg resetPassModel
                |> updateWith ResetPassword GotResetPasswordMsg

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        ( GotDocNewMsg docNewMsg, DocNew docNewModel ) ->
            Page.DocNew.update docNewMsg docNewModel
                |> updateWith DocNew GotDocNewMsg

        ( GotDocMsg docMsg, Doc docModel ) ->
            Page.Doc.update docMsg docModel
                |> updateWith Doc GotDocMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


withCmd : Cmd Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
withCmd newCmd ( model, cmd ) =
    ( model, Cmd.batch [ newCmd, cmd ] )



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

        ResetPassword resetPass ->
            { title = "Gingko - Reset Password", body = [ Html.map GotResetPasswordMsg (Page.ResetPassword.view resetPass) ] }

        Home home ->
            { title = "Gingko - Home", body = [ Html.map GotHomeMsg (Page.Home.view home) ] }

        DocNew _ ->
            { title = "Gingko - New", body = [ Html.div [] [ Html.text "LOADING..." ] ] }

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

        ResetPassword pageModel ->
            Sub.map GotResetPasswordMsg (Page.ResetPassword.subscriptions pageModel)

        Login pageModel ->
            Sub.map GotLoginMsg (Page.Login.subscriptions pageModel)

        Home pageModel ->
            Sub.map GotHomeMsg (Page.Home.subscriptions pageModel)

        DocNew _ ->
            Sub.none

        Doc pageModel ->
            Sub.map GotDocMsg (Page.Doc.subscriptions pageModel)



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
