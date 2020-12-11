module Page.ResetPassword exposing (Model, Msg, init, subscriptions, toUser, update, view)

import Browser.Dom
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, href, id, placeholder, src, type_, value)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http exposing (Error(..))
import Session exposing (Session)
import Task
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { user : Session
    , password : String
    , passwordConfirm : String
    , resetToken : String
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Password
    | PasswordConfirm


init : Session -> String -> ( Model, Cmd Msg )
init user resetToken =
    ( { user = user, resetToken = resetToken, password = "", passwordConfirm = "", errors = [] }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-password"
    )


toUser : Model -> Session
toUser { user } =
    user



-- UPDATE


type Msg
    = NoOp
    | SubmittedForm
    | EnteredPassword String
    | EnteredPassConfirm String
    | Blurred Field
    | CompletedResetPassword (Result Http.Error Session)
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
                    , sendResetPasswordRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        Blurred field ->
            let
                validator =
                    case field of
                        Password ->
                            passwordValidator

                        PasswordConfirm ->
                            passwordConfirmValidator

                        Form ->
                            ifTrue (always True) ( Form, "" )
            in
            case validate validator model of
                Ok _ ->
                    ( { model | errors = [] }, Cmd.none )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        EnteredPassConfirm passwordConfirm ->
            ( { model | passwordConfirm = passwordConfirm }, Cmd.none )

        CompletedResetPassword (Ok user) ->
            ( model, Session.storeLogin user )

        CompletedResetPassword (Err error) ->
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
            ( { model | user = user }, Nav.replaceUrl (Session.navKey user) "/" )


passwordValidator : Validator ( Field, String ) Model
passwordValidator =
    Validate.firstError
        [ ifBlank .password ( Password, "Please enter a password." )
        , ifTrue (\model -> String.length model.password < 7) ( Password, "Password should be 7 characters or more." )
        ]


passwordConfirmValidator : Validator ( Field, String ) Model
passwordConfirmValidator =
    Validate.firstError
        [ ifBlank .passwordConfirm ( PasswordConfirm, "Please enter your password twice." )
        , ifFalse (\m -> m.password == m.passwordConfirm) ( PasswordConfirm, "Passwords do not match." )
        ]


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ passwordValidator
        , passwordConfirmValidator
        ]


sendResetPasswordRequest : Valid Model -> Cmd Msg
sendResetPasswordRequest validModel =
    let
        { password, resetToken, user } =
            Validate.fromValid validModel
    in
    Session.requestResetPassword CompletedResetPassword { newPassword = password, token = resetToken } user



-- VIEW


view : Model -> Html Msg
view model =
    let
        formErrors =
            getFieldErrors Form model.errors

        passwordErrors =
            getFieldErrors Password model.errors

        passwordConfirmErrors =
            getFieldErrors PasswordConfirm model.errors
    in
    div [ id "form-page" ]
        [ div [ class "brand" ]
            [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
            , h1 [] [ text "Gingko" ]
            ]
        , div [ class "page-bg" ] []
        , h1 [ class "headline" ] [ text "Write better, faster." ]
        , div [ class "header" ] [ span [ class "alt-action" ] [ text "Already have an account? ", a [ href "/login" ] [ text "Login" ] ] ]
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ div [] [ text (String.join "\n" formErrors) ]
                , div [ class "input-error" ] [ text (String.join "\n" passwordErrors) ]
                , input
                    [ id "signup-password"
                    , placeholder "Password (min. 7 characters)"
                    , onInput EnteredPassword
                    , type_ "password"
                    , value model.password
                    , onBlur (Blurred Password)
                    ]
                    []
                , div [ class "input-error" ] [ text (String.join "\n" passwordConfirmErrors) ]
                , input
                    [ id "signup-password-confirm"
                    , placeholder "Confirm Password"
                    , onInput EnteredPassConfirm
                    , type_ "password"
                    , value model.passwordConfirm
                    , onBlur (Blurred PasswordConfirm)
                    ]
                    []
                , button [ class "cta" ] [ text "Reset Password" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.loginChanges GotUser (Session.navKey model.user)
