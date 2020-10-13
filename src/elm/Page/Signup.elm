module Page.Signup exposing (Model, Msg, init, subscriptions, toUser, update, view)

import Browser.Dom
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Task
import User exposing (User)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifFalse, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { user : User
    , email : String
    , password : String
    , passwordConfirm : String
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Email
    | Password
    | PasswordConfirm


init : User -> ( Model, Cmd Msg )
init user =
    ( { user = user, email = "", password = "", passwordConfirm = "", errors = [] }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-email"
    )


toUser : Model -> User
toUser { user } =
    user



-- UPDATE


type Msg
    = NoOp
    | SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | EnteredPassConfirm String
    | CompletedSignup (Result Http.Error User)
    | GotUser User


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

        EnteredPassConfirm passwordConfirm ->
            ( { model | passwordConfirm = passwordConfirm }, Cmd.none )

        CompletedSignup (Ok user) ->
            ( model, User.storeSignup user )

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
            ( { model | errors = [ errorMsg ], password = "", passwordConfirm = "" }, Cmd.none )

        GotUser user ->
            ( { model | user = user }, Nav.replaceUrl (User.navKey user) "/" )


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Please enter an email address." )
            , ifInvalidEmail .email (\eml -> ( Email, eml ++ " does not seem to be a valid email." ))
            ]
        , ifBlank .password ( Password, "Please enter a password." )
        , Validate.firstError
            [ ifBlank .passwordConfirm ( PasswordConfirm, "Please enter your password twice." )
            , ifFalse (\m -> m.password == m.passwordConfirm) ( PasswordConfirm, "Passwords do not match." )
            ]
        ]


sendSignupRequest : Valid Model -> Cmd Msg
sendSignupRequest validModel =
    let
        { email, password, user } =
            Validate.fromValid validModel
    in
    User.requestSignup CompletedSignup email password user



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

        passwordConfirmErrors =
            getFieldErrors PasswordConfirm model.errors
    in
    div [ id "form-page" ]
        [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ div [] [ text (String.join "\n" formErrors) ]
                , label [] [ text "Email" ]
                , div [] [ text (String.join "\n" emailErrors) ]
                , input
                    [ id "signup-email"
                    , onInput EnteredEmail
                    , type_ "email"
                    , value model.email
                    , autofocus True
                    ]
                    []
                , label [] [ text "Password" ]
                , div [] [ text (String.join "\n" passwordErrors) ]
                , input
                    [ id "signup-password"
                    , onInput EnteredPassword
                    , type_ "password"
                    , value model.password
                    ]
                    []
                , label [] [ text "Confirm Password" ]
                , div [] [ text (String.join "\n" passwordConfirmErrors) ]
                , input
                    [ id "signup-password-confirm"
                    , onInput EnteredPassConfirm
                    , type_ "password"
                    , value model.passwordConfirm
                    ]
                    []
                , button [] [ text "Signup" ]
                , span [ class "alt-action" ] [ text "or ", a [ href "/login" ] [ text "Login" ] ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    User.loginChanges GotUser (User.navKey model.user)
