module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Doc.UI as UI
import Html
import Json.Decode as Dec exposing (Decoder, Value)
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


type alias Model =
    { state : State
    , navKey : Nav.Key
    }


type State
    = Redirect Session
    | NotFound Session
    | PaymentSuccess Session
    | Signup Page.Signup.Model
    | Login Page.Login.Model
    | ForgotPassword Page.ForgotPassword.Model
    | ResetPassword Page.ResetPassword.Model
    | Copy Page.Copy.Model
    | Import Page.Import.Model
    | DocNew Page.DocNew.Model
    | App Page.App.Model


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init json url navKey =
    let
        user =
            Session.decode json

        changeRouteWithKey =
            changeRouteTo navKey

        addNavKey =
            Tuple.mapFirst (\state -> Model state navKey)
    in
    case ( Session.loggedIn user, Route.fromUrl url ) of
        ( True, route_ ) ->
            changeRouteWithKey route_ (Redirect user)
                |> addNavKey

        ( False, Just Route.Login ) ->
            changeRouteWithKey (Just Route.Login) (Redirect user)
                |> addNavKey

        ( False, Just (Route.ForgotPassword token) ) ->
            changeRouteWithKey (Just (Route.ForgotPassword token)) (Redirect user)
                |> addNavKey

        ( False, Just (Route.ResetPassword token) ) ->
            changeRouteWithKey (Just (Route.ResetPassword token)) (Redirect user)
                |> addNavKey

        ( False, _ ) ->
            changeRouteWithKey (Just Route.Signup) (Redirect user)
                |> addNavKey


changeRouteTo : Nav.Key -> Maybe Route -> State -> ( State, Cmd Msg )
changeRouteTo navKey maybeRoute model =
    let
        user =
            toSession model
    in
    if Session.loggedIn user then
        case maybeRoute of
            Just Route.Root ->
                Page.App.init navKey user Nothing |> updateWith App GotAppMsg

            Just Route.Signup ->
                Page.Signup.init navKey user |> updateWith Signup GotSignupMsg

            Just Route.Login ->
                Page.App.init navKey user Nothing |> updateWith App GotAppMsg

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init navKey user email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init navKey user token
                    |> updateWith ResetPassword GotResetPasswordMsg

            Just Route.EmailConfirmed ->
                let
                    newSession =
                        Session.confirmEmail user
                in
                ( Redirect newSession, Route.replaceUrl navKey Route.Root )

            Just Route.DocNew ->
                Page.DocNew.init navKey user |> updateWith DocNew GotDocNewMsg

            Just (Route.DocUntitled dbName) ->
                let
                    isNew =
                        case model of
                            DocNew _ ->
                                True

                            _ ->
                                False
                in
                Page.App.init navKey user (Just { dbName = dbName, isNew = isNew }) |> updateWith App GotAppMsg

            Just (Route.Doc dbName _) ->
                Page.App.init navKey user (Just { dbName = dbName, isNew = False }) |> updateWith App GotAppMsg

            Just (Route.Copy dbName) ->
                Page.Copy.init navKey user dbName |> updateWith Copy GotCopyMsg

            Just (Route.Import template) ->
                Page.Import.init navKey user template
                    |> updateWith Import GotImportMsg

            Just (Route.Upgrade isOk) ->
                if isOk then
                    ( PaymentSuccess user, Cmd.none )

                else
                    ( Redirect user, Cmd.none )

            Nothing ->
                ( NotFound user, Cmd.none )

    else
        let
            ( signupModel, signupCmds ) =
                Page.Signup.init navKey user
                    |> updateWith Signup GotSignupMsg

            ( loginModel, loginCmds ) =
                Page.Login.init navKey user
                    |> updateWith Login GotLoginMsg
        in
        case maybeRoute of
            Just Route.Signup ->
                ( signupModel, Cmd.batch [ signupCmds, Route.replaceUrl navKey Route.Signup ] )

            Just Route.Login ->
                ( loginModel, loginCmds )

            Just (Route.ForgotPassword email_) ->
                Page.ForgotPassword.init navKey user email_
                    |> updateWith ForgotPassword GotForgotPasswordMsg

            Just (Route.ResetPassword token) ->
                Page.ResetPassword.init navKey user token
                    |> updateWith ResetPassword GotResetPasswordMsg

            _ ->
                ( loginModel, Cmd.batch [ loginCmds, Route.replaceUrl navKey Route.Login ] )


toSession : State -> Session
toSession page =
    case page of
        Redirect user ->
            user

        NotFound user ->
            user

        PaymentSuccess user ->
            user

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



-- UPDATE


type Msg
    = NoOp
    | ChangedUrl Url
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


update : Nav.Key -> Msg -> State -> ( State, Cmd Msg )
update navKey msg state =
    case ( msg, state ) of
        ( ChangedUrl url, Signup _ ) ->
            case Route.fromUrl url of
                Just Route.Signup ->
                    ( state, Cmd.none )

                otherRoute ->
                    changeRouteTo navKey otherRoute state

        ( ChangedUrl url, _ ) ->
            changeRouteTo navKey (Route.fromUrl url) state

        ( ClickedLink urlRequest, App appModel ) ->
            case urlRequest of
                Browser.Internal url ->
                    if Page.App.isDirty appModel then
                        let
                            saveShortcut =
                                if Session.isMac (toSession state) then
                                    "⌘+enter"

                                else
                                    "Ctrl+Enter"
                        in
                        ( state, send <| Alert ("You have unsaved changes!\n" ++ saveShortcut ++ " to save.") )

                    else
                        ( state, Nav.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( state, Nav.load href )

        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( state, Nav.pushUrl navKey (Url.toString url) )

                Browser.External href ->
                    ( state, Nav.load href )

        ( SettingsChanged json, PaymentSuccess session ) ->
            ( PaymentSuccess (Session.sync json session), Cmd.none )

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
            ( state, Cmd.none )


updateWith : (subModel -> State) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( State, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


updateModel : Msg -> Model -> ( Model, Cmd Msg )
updateModel msg model =
    update model.navKey msg model.state |> Tuple.mapFirst (\s -> Model s model.navKey)


withCmd : Cmd Msg -> ( State, Cmd Msg ) -> ( State, Cmd Msg )
withCmd newCmd ( model, cmd ) =
    ( model, Cmd.batch [ newCmd, cmd ] )



-- VIEW


view : State -> Document Msg
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
subscriptions { state } =
    case state of
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
        , view = .state >> view
        , update = updateModel
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
