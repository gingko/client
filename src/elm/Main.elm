module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Import.Template as Template
import Json.Decode exposing (Decoder, Value)
import Page.Doc
import Page.DocNew
import Page.Empty
import Page.ForgotPassword
import Page.Home
import Page.Import
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
    | ForgotPassword Page.ForgotPassword.Model
    | ResetPassword Page.ResetPassword.Model
    | Home Page.Home.Model
    | Empty Page.Empty.Model
    | Import Page.Import.Model
    | DocNew User
    | Doc Page.Doc.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        user =
            User.decode navKey json
    in
    case ( User.loggedIn user, Route.fromUrl url ) of
        ( True, route_ ) ->
            changeRouteTo route_ (Redirect user)

        ( False, Just Route.Login ) ->
            changeRouteTo (Just Route.Login) (Redirect user)

        ( False, Just (Route.ForgotPassword token) ) ->
            changeRouteTo (Just (Route.ForgotPassword token)) (Redirect user)

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
            Just Route.Root ->
                Page.Empty.init user |> updateWith Empty GotEmptyMsg

            Just Route.Signup ->
                Page.Signup.init user |> updateWith Signup GotSignupMsg

            Just Route.Login ->
                Page.Home.init user |> updateWith Home GotHomeMsg

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init user email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init user token
                    |> updateWith ResetPassword GotResetPasswordMsg

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

            Just (Route.Import template) ->
                Page.Import.init user template
                    |> updateWith Import GotImportMsg

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

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init user email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

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

        ForgotPassword forgot ->
            Page.ForgotPassword.toUser forgot

        ResetPassword reset ->
            Page.ResetPassword.toUser reset

        Home home ->
            Page.Home.toUser home

        Empty home ->
            Page.Empty.toUser home

        Import user ->
            user

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
    | GotForgotPasswordMsg Page.ForgotPassword.Msg
    | GotResetPasswordMsg Page.ResetPassword.Msg
    | GotEmptyMsg Page.Empty.Msg
    | GotHomeMsg Page.Home.Msg
    | GotImportMsg Page.Import.Msg
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

        ( GotForgotPasswordMsg forgotPassMsg, ForgotPassword forgotPassModel ) ->
            Page.ForgotPassword.update forgotPassMsg forgotPassModel
                |> updateWith ForgotPassword GotForgotPasswordMsg

        ( GotResetPasswordMsg resetPassMsg, ResetPassword resetPassModel ) ->
            Page.ResetPassword.update resetPassMsg resetPassModel
                |> updateWith ResetPassword GotResetPasswordMsg

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        ( GotEmptyMsg emptyMsg, Empty emptyModel ) ->
            Page.Empty.update emptyMsg emptyModel
                |> updateWith Empty GotEmptyMsg

        ( GotImportMsg homeMsg, Import homeModel ) ->
            Page.Import.update homeMsg homeModel
                |> updateWith Import GotImportMsg

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

        ForgotPassword forgotPass ->
            { title = "Gingko - Forgot Password", body = [ Html.map GotForgotPasswordMsg (Page.ForgotPassword.view forgotPass) ] }

        ResetPassword resetPass ->
            { title = "Gingko - Reset Password", body = [ Html.map GotResetPasswordMsg (Page.ResetPassword.view resetPass) ] }

        Home home ->
            { title = "Gingko - Home", body = [ Html.map GotHomeMsg (Page.Home.view home) ] }

        Empty empty ->
            { title = "Gingko - Empty", body = [ Html.map GotEmptyMsg (Page.Empty.view empty) ] }

        Import importModel ->
            { title = "Importing...", body = [ Html.div [] [ Html.text "Importing..." ] ] }

        DocNew _ ->
            { title = "Gingko - New", body = [ Html.div [] [ Html.text "LOADING..." ] ] }

        Doc doc ->
            { title = Page.Doc.getTitle doc ++ " - Gingko", body = [ Html.map GotDocMsg (Page.Doc.view doc) ] }



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

        ForgotPassword pageModel ->
            Sub.map GotForgotPasswordMsg (Page.ForgotPassword.subscriptions pageModel)

        ResetPassword pageModel ->
            Sub.map GotResetPasswordMsg (Page.ResetPassword.subscriptions pageModel)

        Login pageModel ->
            Sub.map GotLoginMsg (Page.Login.subscriptions pageModel)

        Home pageModel ->
            Sub.map GotHomeMsg (Page.Home.subscriptions pageModel)

        Empty pageModel ->
            Sub.map GotEmptyMsg (Page.Empty.subscriptions pageModel)

        Import pageModel ->
            Sub.map GotImportMsg (Page.Import.subscriptions pageModel)

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
