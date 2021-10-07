module Page.Signup exposing (Model, Msg, init, subscriptions, toUser, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom
import Html exposing (..)
import Html.Attributes exposing (autocomplete, autofocus, class, classList, for, href, id, src, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http exposing (Error(..))
import Import.Template as Template
import Route
import Session exposing (Session)
import Svg.Attributes
import Task
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { user : Session
    , email : String
    , password : String
    , showPassword : Bool
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Email
    | Password


init : Session -> ( Model, Cmd Msg )
init user =
    ( { user = user, email = "", password = "", showPassword = False, errors = [] }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-email"
    )


toUser : Model -> Session
toUser { user } =
    user



-- UPDATE


type Msg
    = NoOp
    | SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | ToggleShowPassword
    | CompletedSignup (Result Http.Error Session)
    | GotUser Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SubmittedForm ->
            case validate modelValidator model of
                Ok validModel ->
                    ( model
                    , sendSignupRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        ToggleShowPassword ->
            ( { model | showPassword = not model.showPassword }, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "signup-password") )

        CompletedSignup (Ok user) ->
            ( { model | user = user }, Session.storeSignup user )

        CompletedSignup (Err error) ->
            let
                fallbackMsg =
                    ( Form, "Server Issue. Something wrong on our end. Please let us know!" )

                errorMsg =
                    case error of
                        Timeout ->
                            ( Form, "Timed out. Maybe there's a server issue?" )

                        NetworkError ->
                            ( Form, "Network Error. Maybe you're offline?" )

                        BadStatus statusCode ->
                            case statusCode of
                                409 ->
                                    ( Form, "Username already exists. Login?" )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ], password = "" }, Cmd.none )

        GotUser user ->
            ( model, Route.replaceUrl (Session.navKey user) (Route.Import Template.WelcomeTree) )


emailValidator : Validator ( Field, String ) Model
emailValidator =
    Validate.firstError
        [ ifBlank .email ( Email, "Please enter an email address." )
        , ifInvalidEmail .email (\eml -> ( Email, eml ++ " does not seem to be a valid email." ))
        ]


passwordValidator : Validator ( Field, String ) Model
passwordValidator =
    Validate.firstError
        [ ifBlank .password ( Password, "Please enter a password." )
        , ifTrue (\model -> String.length model.password < 7) ( Password, "Password should be 7 characters or more." )
        ]


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ emailValidator
        , passwordValidator
        ]


sendSignupRequest : Valid Model -> Cmd Msg
sendSignupRequest validModel =
    let
        { email, password, user } =
            Validate.fromValid validModel
    in
    Session.requestSignup CompletedSignup email password user



-- VIEW


view : Model -> Html Msg
view model =
    let
        formErrors =
            getFieldErrors Form model.errors

        emailErrors =
            getFieldErrors Email model.errors

        passwordErrors =
            getFieldErrors Password model.errors

        showHidePassword =
            if model.showPassword then
                div [ id "show-hide-password", onClick ToggleShowPassword ] [ AntIcons.eyeInvisibleOutlined [ Svg.Attributes.class "icon" ], text "Hide" ]

            else
                div [ id "show-hide-password", onClick ToggleShowPassword ] [ AntIcons.eyeOutlined [ Svg.Attributes.class "icon" ], text "Show" ]
    in
    div [ id "form-page" ]
        [ div [ class "page-backdrop" ] []
        , div [ class "page-bg" ] []
        , a [ class "brand", href "{%HOMEPAGE_URL%}" ] [ img [ id "logo", src "gingko-leaf-logo.svg" ] [] ]
        , div [ class "form-header-container" ]
            [ h1 [ class "headline" ] [ text "Welcome to Gingko Writer" ]
            , p [ class "subtitle" ] [ text "Write the way you think, organize as you go, and let your words flow." ]
            ]
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ div [] [ text (String.join "\n" formErrors) ]
                , div [ class "input-error" ] [ text (String.join "\n" emailErrors) ]
                , label [ for "singup-email" ] [ text "Email" ]
                , input
                    [ id "signup-email"
                    , classList [ ( "has-error", List.length emailErrors > 0 ) ]
                    , onInput EnteredEmail
                    , type_ "email"
                    , value model.email
                    , autofocus True
                    , autocomplete True
                    ]
                    []
                , div [ class "input-error" ] [ text (String.join "\n" passwordErrors) ]
                , label [ for "singup-password" ] [ text "Password (7+ characters)", showHidePassword ]
                , input
                    [ id "signup-password"
                    , onInput EnteredPassword
                    , type_
                        (if model.showPassword then
                            "text"

                         else
                            "password"
                        )
                    , value model.password
                    , autocomplete True
                    ]
                    []
                , button [ id "signup-button", class "cta" ] [ text "Start Writing" ]
                , div [ id "post-cta-divider" ] [ hr [] [], div [] [ text "or" ], hr [] [] ]
                , span [ class "alt-action" ] [ text "Already have an account? ", a [ href "/login" ] [ text "Login" ] ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.loginChanges GotUser (Session.navKey model.user)
