module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Import
import Json.Decode as Dec exposing (Decoder, Value)
import Page.Doc
import Page.Home
import Page.Login
import Page.NotFound
import Page.Signup
import Route exposing (Route)
import Session exposing (Session)
import Translation exposing (Language, langFromString)
import Url exposing (Url)



-- MODEL


type Model
    = Redirect Session
    | NotFound Session
    | Signup Page.Signup.Model
    | Login Page.Login.Model
    | Home Page.Home.Model
    | Doc Page.Doc.Model


type alias Flags =
    { session : Maybe String
    , language : Language
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsData url navKey =
    let
        flagsDecoder : Decoder Flags
        flagsDecoder =
            Dec.map2 (\s ls -> Flags s (langFromString ls))
                (Dec.field "session" (Dec.nullable Dec.string))
                (Dec.field "language" Dec.string)
    in
    case Dec.decodeValue flagsDecoder flagsData of
        Ok flags ->
            changeRouteTo (Route.fromUrl url) (Redirect (Session.fromData navKey flags.session))

        Err err ->
            Debug.todo "flags decoding error" err


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    if Session.loggedIn session then
        case ( model, maybeRoute ) of
            ( _, Just Route.Home ) ->
                Page.Home.init session |> updateWith Home GotHomeMsg

            ( _, Just Route.Signup ) ->
                Page.Signup.init session |> updateWith Signup GotSignupMsg

            ( _, Just Route.Login ) ->
                Page.Login.init session |> updateWith Login GotLoginMsg

            ( Home _, Just (Route.DocNew dbName) ) ->
                Page.Doc.init session dbName True |> updateWith Doc GotDocMsg

            ( _, Just (Route.DocNew _) ) ->
                ( model, Cmd.none )

            ( Doc _, Just (Route.DocUntitled _) ) ->
                ( model, Cmd.none )

            ( _, Just (Route.DocUntitled dbName) ) ->
                Page.Doc.init session dbName False |> updateWith Doc GotDocMsg

            ( _, Just (Route.Doc dbName _) ) ->
                Page.Doc.init session dbName False |> updateWith Doc GotDocMsg

            ( _, Nothing ) ->
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
                ( loginModel, Cmd.batch [ loginCmds, Route.replaceUrl (Session.navKey session) Route.Login ] )


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

        Home pageModel ->
            Sub.map GotHomeMsg (Page.Home.subscriptions pageModel)

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
