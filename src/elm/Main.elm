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
import Page.Public
import Page.ResetPassword
import Page.Signup
import Public
import Route
import Session exposing (LoggedIn, Session(..))
import Url exposing (Url)



-- MODEL


type alias WebSessionData =
    { globalData : GlobalData
    , session : LoggedIn
    , navKey : Nav.Key
    }


type Model
    = -- Logged Out Pages:
      Signup Page.Signup.Model
    | Login Page.Login.Model
    | ForgotPassword Page.ForgotPassword.Model
    | ResetPassword Page.ResetPassword.Model
      -- Logged In Pages:
    | PaymentSuccess WebSessionData
    | Copy Page.Copy.Model
    | Import Page.Import.Model
    | DocNew Page.DocNew.Model
    | App Page.App.Model
      -- Public Pages:
    | Public Page.Public.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        isPublic =
            Public.isPublic url

        session =
            Session.decode json

        globalData =
            GlobalData.decode json

        ( initModel, _ ) =
            case session of
                LoggedInSession loggedInSession ->
                    Page.App.init navKey globalData loggedInSession Nothing |> updateWith App GotAppMsg

                GuestSession guestSession ->
                    Page.Login.init navKey globalData guestSession |> updateWith Login GotLoginMsg
    in
    if isPublic then
        case .path (AppUrl.fromUrl url) of
            [] ->
                Page.Public.init navKey ""
                    |> updateWith Public GotPublicMsg

            [ dbName ] ->
                Page.Public.init navKey dbName
                    |> updateWith Public GotPublicMsg

            _ ->
                handleUrlChange url initModel

    else
        handleUrlChange url initModel


replaceUrl : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
replaceUrl path ( model, cmd ) =
    ( model, Cmd.batch [ cmd, Nav.replaceUrl (getNavKey model) path ] )


handleUrlChange : Url -> Model -> ( Model, Cmd Msg )
handleUrlChange url model =
    let
        navKey =
            getNavKey model

        globalData =
            toGlobalData model

        appUrl =
            AppUrl.fromUrl url
    in
    case toSession model of
        LoggedInSession session ->
            case appUrl.path of
                [] ->
                    if loginInProgress model == Nothing then
                        Page.App.init navKey globalData session Nothing |> updateWith App GotAppMsg

                    else
                        ( model, Cmd.none )

                [ "new" ] ->
                    Page.DocNew.init navKey globalData session |> updateWith DocNew GotDocNewMsg

                [ "import", templateName ] ->
                    case Template.fromString templateName of
                        Just template ->
                            Page.Import.init navKey globalData session template
                                |> updateWith Import GotImportMsg

                        Nothing ->
                            Page.App.init navKey globalData session Nothing
                                |> updateWith App GotAppMsg

                [ "copy", dbName ] ->
                    Page.Copy.init navKey globalData session dbName
                        |> updateWith Copy GotCopyMsg

                [ "upgrade", "success" ] ->
                    ( PaymentSuccess { globalData = globalData, session = session, navKey = navKey }
                    , Cmd.none
                    )

                [ "confirm" ] ->
                    case model of
                        App appModel ->
                            ( Page.App.updateSession
                                (Session.confirmEmail (GlobalData.currentTime globalData) session)
                                appModel
                                |> App
                            , Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

                [ "login" ] ->
                    ( model, Route.pushUrl navKey Route.Root )

                [ "signup" ] ->
                    ( model, Route.pushUrl navKey Route.Root )

                [ dbName ] ->
                    let
                        isNew =
                            case model of
                                DocNew _ ->
                                    True

                                _ ->
                                    False
                    in
                    Page.App.init navKey globalData session (Just { dbName = dbName, isNew = isNew })
                        |> updateWith App GotAppMsg

                [ dbName, "404-not-found" ] ->
                    Page.App.notFound navKey globalData session |> updateWith App GotAppMsg

                [ dbName, _ ] ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GuestSession guestSession ->
            case appUrl.path of
                [] ->
                    case loginInProgress model of
                        Just loggingIn ->
                            Page.App.init navKey globalData loggingIn Nothing |> updateWith App GotAppMsg

                        Nothing ->
                            Page.Signup.init navKey globalData guestSession
                                |> updateWith Signup GotSignupMsg
                                |> replaceUrl "signup"

                [ "login" ] ->
                    Page.Login.init navKey globalData guestSession |> updateWith Login GotLoginMsg

                [ "signup" ] ->
                    Page.Signup.init navKey globalData guestSession |> updateWith Signup GotSignupMsg

                [ "import", templateName ] ->
                    case loginInProgress model of
                        Just loggingIn ->
                            case Template.fromString templateName of
                                Just template ->
                                    Page.Import.init navKey globalData loggingIn template
                                        |> updateWith Import GotImportMsg

                                Nothing ->
                                    Page.App.init navKey globalData loggingIn Nothing
                                        |> updateWith App GotAppMsg

                        Nothing ->
                            ( model, Cmd.none )

                [ "forgot-password" ] ->
                    Page.ForgotPassword.init navKey globalData guestSession (Dict.get "email" appUrl.queryParameters |> Maybe.andThen List.head)
                        |> updateWith ForgotPassword GotForgotPasswordMsg

                [ "reset-password", token ] ->
                    Page.ResetPassword.init navKey globalData guestSession token
                        |> updateWith ResetPassword GotResetPasswordMsg

                _ ->
                    ( model, Cmd.none )


loginInProgress : Model -> Maybe LoggedIn
loginInProgress model =
    case model of
        Signup signup ->
            Page.Signup.transition signup

        Login login ->
            Page.Login.transition login

        ForgotPassword forgot ->
            Page.ForgotPassword.transition forgot

        ResetPassword reset ->
            Page.ResetPassword.transition reset

        _ ->
            Nothing


toSession : Model -> Session
toSession model =
    case model of
        PaymentSuccess { session } ->
            session |> LoggedInSession

        Signup signup ->
            Page.Signup.toSession signup

        Login login ->
            Page.Login.toSession login

        ForgotPassword forgot ->
            Page.ForgotPassword.toSession forgot

        ResetPassword reset ->
            Page.ResetPassword.toSession reset

        Copy copy ->
            Page.Copy.toSession copy

        Import importModel ->
            Page.Import.toSession importModel

        DocNew docNew ->
            Page.DocNew.toSession docNew

        App appModel ->
            Page.App.toSession appModel

        Public _ ->
            Session.public


toGlobalData : Model -> GlobalData
toGlobalData model =
    case model of
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

        Public _ ->
            GlobalData.public


getNavKey : Model -> Nav.Key
getNavKey model =
    case model of
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

        Public publicModel ->
            Page.Public.navKey publicModel



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
    | GotPublicMsg Page.Public.Msg
    | UserLoggedOut


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        navKey =
            getNavKey model

        globalData =
            toGlobalData model
    in
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            handleUrlChange url model

        ( ClickedLink urlRequest, App appModel ) ->
            case urlRequest of
                Browser.Internal url ->
                    if Page.App.isDirty appModel then
                        let
                            saveShortcut =
                                if GlobalData.isMac globalData then
                                    "⌘+enter"

                                else
                                    "Ctrl+Enter"
                        in
                        ( model, send <| Alert ("You have unsaved changes!\n" ++ saveShortcut ++ " to save.") )

                    else
                        ( model, Nav.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( SettingsChanged json, PaymentSuccess webSessionData ) ->
            ( PaymentSuccess
                { webSessionData
                    | session =
                        Session.sync
                            json
                            (GlobalData.currentTime webSessionData.globalData)
                            webSessionData.session
                }
            , Cmd.none
            )

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

        ( UserLoggedOut, _ ) ->
            case toSession model of
                LoggedInSession session ->
                    Page.Login.init navKey globalData (Session.toGuest session)
                        |> updateWith Login GotLoginMsg
                        |> replaceUrl "/login"

                _ ->
                    ( model, Cmd.none )

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

        Public publicModel ->
            { title = "Gingko Writer", body = [ Html.map GotPublicMsg (Page.Public.view publicModel) ] }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
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

        Public _ ->
            Sub.none


globalSubscriptions : Model -> Sub Msg
globalSubscriptions model =
    Sub.batch
        [ subscriptions model
        , Session.userLoggedOut UserLoggedOut
        ]



-- MAIN


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = globalSubscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
