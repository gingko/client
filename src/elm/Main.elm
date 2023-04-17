module Main exposing (main)

import AppUrl
import Browser exposing (Document)
import Browser.Navigation as Nav
import Dict
import Doc.UI as UI
import GlobalData exposing (GlobalData)
import Html
import Import.Template as Template
import Json.Decode as Dec exposing (Value)
import Outgoing exposing (Msg(..), send)
import Page.App
import Page.Copy
import Page.DocNew
import Page.ForgotPassword
import Page.Import
import Page.Login
import Page.Message
import Page.NotFound
import Page.ResetPassword
import Page.Signup
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- MODEL


type alias WebSessionData =
    { globalData : GlobalData
    , session : Session
    , navKey : Nav.Key
    }


type Model
    = Redirect WebSessionData
      -- Logged Out Pages:
    | Signup Page.Signup.Model
    | Login Page.Login.Model
    | ForgotPassword Page.ForgotPassword.Model
    | ResetPassword Page.ResetPassword.Model
      -- Logged In Pages:
    | NotFound WebSessionData
    | PaymentSuccess WebSessionData
    | Copy Page.Copy.Model
    | Import Page.Import.Model
    | DocNew Page.DocNew.Model
    | App Page.App.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        session =
            Session.decode json

        globalData =
            GlobalData.decode json

        webSessionData =
            { globalData = globalData, session = session, navKey = navKey }

        appUrl =
            AppUrl.fromUrl url
    in
    if Session.loggedIn session then
        case appUrl.path of
            [ "new" ] ->
                Page.DocNew.init navKey globalData session |> updateWith DocNew GotDocNewMsg

            [ "confirm" ] ->
                let
                    newSession =
                        Session.confirmEmail (GlobalData.currentTime globalData) session
                in
                Page.App.init navKey globalData newSession Nothing |> updateWith App GotAppMsg

            [ "copy", dbName ] ->
                Page.Copy.init navKey globalData session dbName |> updateWith Copy GotCopyMsg

            [ "import", templateName ] ->
                case Template.fromString templateName of
                    Just template ->
                        Page.Import.init navKey globalData session template |> updateWith Import GotImportMsg

                    Nothing ->
                        Page.App.init navKey globalData session Nothing |> updateWith App GotAppMsg

            [ dbName ] ->
                Page.App.init navKey globalData session (Just { dbName = dbName, isNew = False }) |> updateWith App GotAppMsg

            [ dbName, _ ] ->
                Page.App.init navKey globalData session (Just { dbName = dbName, isNew = False }) |> updateWith App GotAppMsg

            _ ->
                Page.App.init navKey globalData session Nothing |> updateWith App GotAppMsg

    else
        case appUrl.path of
            [] ->
                Page.Signup.init navKey globalData session
                    |> updateWith Signup GotSignupMsg
                    |> replaceUrl "signup"

            [ "login" ] ->
                Page.Login.init navKey globalData session |> updateWith Login GotLoginMsg

            [ "signup" ] ->
                Page.Signup.init navKey globalData session |> updateWith Signup GotSignupMsg

            [ "forgot-password" ] ->
                Page.ForgotPassword.init navKey globalData session (Dict.get "email" appUrl.queryParameters |> Maybe.andThen List.head)
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            [ "reset-password", token ] ->
                Page.ResetPassword.init navKey globalData session token
                    |> updateWith ResetPassword GotResetPasswordMsg

            _ ->
                Page.Login.init navKey globalData session
                    |> updateWith Login GotLoginMsg
                    |> replaceUrl "login"


replaceUrl : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
replaceUrl path ( model, cmd ) =
    ( model, Cmd.batch [ cmd, Nav.replaceUrl (getNavKey model) path ] )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        navKey =
            getNavKey model

        globalData =
            toGlobalData model

        session =
            toSession model
    in
    if Session.loggedIn session then
        case maybeRoute of
            Just Route.Root ->
                Page.App.init navKey globalData session Nothing |> updateWith App GotAppMsg

            Just Route.Signup ->
                Page.Signup.init navKey globalData session |> updateWith Signup GotSignupMsg

            Just Route.Login ->
                Page.App.init navKey globalData session Nothing |> updateWith App GotAppMsg

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init navKey globalData session email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init navKey globalData session token
                    |> updateWith ResetPassword GotResetPasswordMsg

            Just Route.EmailConfirmed ->
                let
                    newSession =
                        Session.confirmEmail (GlobalData.currentTime globalData) session
                in
                ( Redirect (WebSessionData globalData newSession navKey), Route.replaceUrl navKey Route.Root )

            Just Route.DocNew ->
                Page.DocNew.init navKey globalData session |> updateWith DocNew GotDocNewMsg

            Just (Route.DocUntitled dbName) ->
                let
                    isNew =
                        case model of
                            DocNew _ ->
                                True

                            _ ->
                                False
                in
                Page.App.init navKey globalData session (Just { dbName = dbName, isNew = isNew }) |> updateWith App GotAppMsg

            Just (Route.Doc dbName _) ->
                Page.App.init navKey globalData session (Just { dbName = dbName, isNew = False }) |> updateWith App GotAppMsg

            Just (Route.Copy dbName) ->
                Page.Copy.init navKey globalData session dbName |> updateWith Copy GotCopyMsg

            Just (Route.Import template) ->
                Page.Import.init navKey globalData session template
                    |> updateWith Import GotImportMsg

            Just (Route.Upgrade isOk) ->
                if isOk then
                    ( PaymentSuccess (WebSessionData globalData session navKey), Cmd.none )

                else
                    ( Redirect (WebSessionData globalData session navKey), Cmd.none )

            Nothing ->
                ( NotFound (WebSessionData globalData session navKey), Cmd.none )

    else
        let
            ( signupModel, signupCmds ) =
                Page.Signup.init navKey globalData session
                    |> updateWith Signup GotSignupMsg

            ( loginModel, loginCmds ) =
                Page.Login.init navKey globalData session
                    |> updateWith Login GotLoginMsg
        in
        case maybeRoute of
            Just Route.Signup ->
                ( signupModel, Cmd.batch [ signupCmds, Route.replaceUrl navKey Route.Signup ] )

            Just Route.Login ->
                ( loginModel, loginCmds )

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init navKey globalData session email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init navKey globalData session token
                    |> updateWith ResetPassword GotResetPasswordMsg

            _ ->
                ( loginModel, Cmd.batch [ loginCmds, Route.replaceUrl navKey Route.Login ] )


toSession : Model -> Session
toSession page =
    case page of
        Redirect { session } ->
            session

        NotFound { session } ->
            session

        PaymentSuccess { session } ->
            session

        Signup signup ->
            Page.Signup.toUser signup

        Login login ->
            Page.Login.toUser login

        ForgotPassword forgot ->
            Page.ForgotPassword.toUser forgot

        ResetPassword reset ->
            Page.ResetPassword.toUser reset

        Copy copy ->
            Page.Copy.toUser copy

        Import importModel ->
            importModel.session

        DocNew docNew ->
            docNew.session

        App appModel ->
            Page.App.toSession appModel


toGlobalData : Model -> GlobalData
toGlobalData model =
    case model of
        Redirect { globalData } ->
            globalData

        NotFound { globalData } ->
            globalData

        PaymentSuccess { globalData } ->
            globalData

        Signup signup ->
            Page.Signup.globalData signup

        Login login ->
            Page.Login.globalData login

        ForgotPassword forgot ->
            Page.ForgotPassword.globalData forgot

        ResetPassword reset ->
            Page.ResetPassword.globalData reset

        Copy copy ->
            Page.Copy.globalData copy

        Import importModel ->
            importModel.globalData

        DocNew docNew ->
            docNew.globalData

        App appModel ->
            Page.App.toGlobalData appModel


getNavKey : Model -> Nav.Key
getNavKey model =
    case model of
        Redirect { navKey } ->
            navKey

        NotFound { navKey } ->
            navKey

        PaymentSuccess { navKey } ->
            navKey

        Signup signup ->
            Page.Signup.navKey signup

        Login login ->
            Page.Login.navKey login

        ForgotPassword forgot ->
            Page.ForgotPassword.navKey forgot

        ResetPassword reset ->
            Page.ResetPassword.navKey reset

        Copy copy ->
            Page.Copy.navKey copy

        Import importModel ->
            importModel.navKey

        DocNew docNew ->
            docNew.navKey

        App appModel ->
            Page.App.navKey appModel



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | SettingsChanged Dec.Value
    | GotSignupMsg Page.Signup.Msg
    | GotLoginMsg Page.Login.Msg
    | GotForgotPasswordMsg Page.ForgotPassword.Msg
    | GotResetPasswordMsg Page.ResetPassword.Msg
    | GotCopyMsg Page.Copy.Msg
    | GotImportMsg Page.Import.Msg
    | GotDocNewMsg Page.DocNew.Msg
    | GotAppMsg Page.App.Msg


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

        ( ClickedLink urlRequest, App appModel ) ->
            case urlRequest of
                Browser.Internal url ->
                    if Page.App.isDirty appModel then
                        let
                            saveShortcut =
                                if GlobalData.isMac (toGlobalData model) then
                                    "⌘+enter"

                                else
                                    "Ctrl+Enter"
                        in
                        ( model, send <| Alert ("You have unsaved changes!\n" ++ saveShortcut ++ " to save.") )

                    else
                        ( model, Nav.pushUrl (getNavKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (getNavKey model) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( SettingsChanged json, PaymentSuccess webSessionData ) ->
            ( PaymentSuccess { webSessionData | session = Session.sync json (GlobalData.currentTime webSessionData.globalData) webSessionData.session }, Cmd.none )

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

        ( GotCopyMsg copyMsg, Copy copyModel ) ->
            Page.Copy.update copyMsg copyModel
                |> updateWith Copy GotCopyMsg

        ( GotImportMsg homeMsg, Import homeModel ) ->
            Page.Import.update homeMsg homeModel
                |> updateWith Import GotImportMsg

        ( GotDocNewMsg docNewMsg, DocNew docNewModel ) ->
            Page.DocNew.update docNewMsg docNewModel
                |> updateWith DocNew GotDocNewMsg

        ( GotAppMsg appMsg, App appModel ) ->
            Page.App.update appMsg appModel
                |> updateWith App GotAppMsg

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
            { title = "Gingko Writer - Loading...", body = [ UI.viewAppLoadingSpinner False ] }

        NotFound _ ->
            Page.NotFound.view

        PaymentSuccess _ ->
            Page.Message.viewSuccess

        Signup signup ->
            { title = "Gingko Writer - Signup", body = [ Html.map GotSignupMsg (Page.Signup.view signup) ] }

        Login login ->
            { title = "Gingko Writer - Login", body = [ Html.map GotLoginMsg (Page.Login.view login) ] }

        ForgotPassword forgotPass ->
            { title = "Gingko - Forgot Password", body = [ Html.map GotForgotPasswordMsg (Page.ForgotPassword.view forgotPass) ] }

        ResetPassword resetPass ->
            { title = "Gingko - Reset Password", body = [ Html.map GotResetPasswordMsg (Page.ResetPassword.view resetPass) ] }

        Copy copyModel ->
            { title = "Duplicating...", body = [ UI.viewAppLoadingSpinner (Session.fileMenuOpen copyModel.session) ] }

        Import importModel ->
            { title = "Importing...", body = [ UI.viewAppLoadingSpinner (Session.fileMenuOpen importModel.session) ] }

        DocNew _ ->
            { title = "Gingko Writer - New", body = [ Html.div [] [ Html.text "LOADING..." ] ] }

        App app ->
            let
                title =
                    case Page.App.getTitle app of
                        Just docTitle ->
                            docTitle ++ " - Gingko Writer"

                        Nothing ->
                            "Gingko Writer"
            in
            { title = title, body = [ Html.map GotAppMsg (Page.App.view app) ] }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Sub.none

        NotFound _ ->
            Sub.none

        PaymentSuccess _ ->
            Session.userSettingsChange SettingsChanged

        Signup pageModel ->
            Sub.map GotSignupMsg (Page.Signup.subscriptions pageModel)

        ForgotPassword pageModel ->
            Sub.map GotForgotPasswordMsg (Page.ForgotPassword.subscriptions pageModel)

        ResetPassword pageModel ->
            Sub.map GotResetPasswordMsg (Page.ResetPassword.subscriptions pageModel)

        Login pageModel ->
            Sub.map GotLoginMsg (Page.Login.subscriptions pageModel)

        Copy pageModel ->
            Sub.map GotCopyMsg (Page.Copy.subscriptions pageModel)

        Import pageModel ->
            Sub.map GotImportMsg (Page.Import.subscriptions pageModel)

        DocNew _ ->
            Sub.none

        App appModel ->
            Sub.map GotAppMsg (Page.App.subscriptions appModel)



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
