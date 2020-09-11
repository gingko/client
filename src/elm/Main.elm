module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Import
import Json.Decode as Dec exposing (Decoder, Value)
import Page.Doc
import Page.DocNew
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
    | DocNew Session
    | Doc Page.Doc.Model


type alias Flags =
    { session : Maybe String
    , seed : Int
    , language : Language
    }


init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flagsData url navKey =
    let
        flagsDecoder =
            Dec.map3 Flags
                (Dec.field "email" (Dec.nullable Dec.string))
                (Dec.field "seed" Dec.int)
                (Dec.field "language" Dec.string |> Dec.map langFromString)
    in
    case Dec.decodeValue flagsDecoder flagsData of
        Ok flags ->
            changeRouteTo (Route.fromUrl url) (Redirect (Session.fromData navKey flags.seed flags.session))

        Err _ ->
            changeRouteTo (Just Route.Login) (Redirect (Session.guest navKey))


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

            Just Route.Logout ->
                Page.Login.init session
                    |> updateWith Login GotLoginMsg
                    |> withCmd Session.logout

            Just Route.DocNew ->
                Page.DocNew.init session |> updateWith DocNew GotDocNewMsg

            Just (Route.DocUntitled dbName) ->
                let
                    isNew =
                        case model of
                            DocNew _ ->
                                True

                            _ ->
                                False
                in
                Page.Doc.init session dbName isNew |> updateWith Doc GotDocMsg

            Just (Route.Doc dbName _) ->
                Page.Doc.init session dbName False |> updateWith Doc GotDocMsg

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

        DocNew session ->
            session

        Doc doc ->
            Page.Doc.toSession doc



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotSignupMsg Page.Signup.Msg
    | GotLoginMsg Page.Login.Msg
    | GotHomeMsg Page.Home.Msg
    | GotDocNewMsg Page.DocNew.Msg
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
