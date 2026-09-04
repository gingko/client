module Page.ResetPassword exposing (Model, Msg, globalData, init, navKey, subscriptions, toSession, transition, update, view)

import Browser.Dom
import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, classList, href, id, placeholder, src, type_, value)
import Html.Events exposing (onBlur, onInput, onSubmit)
import Http exposing (Error(..))
import Session exposing (Guest, LoggedIn, Session(..))
import Task
import Translation exposing (Language)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifFalse, ifInvalidEmail, ifTrue, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Guest
    , transition : Maybe LoggedIn
    , navKey : Nav.Key
    , password : String
    , passwordConfirm : String
    , resetToken : String
    , errors : List ( Field, String )
    }


type Field
    = Form
    | Password
    | PasswordConfirm


init : Nav.Key -> GlobalData -> Guest -> String -> ( Model, Cmd Msg )
init nKey gData session resetToken =
    ( { globalData = gData
      , session = session
      , transition = Nothing
      , navKey = nKey
      , resetToken = resetToken
      , password = ""
      , passwordConfirm = ""
      , errors = []
      }
    , Task.attempt (\_ -> NoOp) <| Browser.Dom.focus "signup-password"
    )


toSession : Model -> Session
toSession { session } =
    session |> GuestSession


transition : Model -> Maybe LoggedIn
transition model =
    model.transition


navKey : Model -> Nav.Key
navKey model =
    model.navKey


globalData : Model -> GlobalData
globalData model =
    model.globalData



-- UPDATE


type Msg
    = NoOp
    | SubmittedForm
    | EnteredPassword String
    | EnteredPassConfirm String
    | Blurred Field
    | CompletedResetPassword (Result Http.Error ( LoggedIn, Language ))
    | UserSaved


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

        CompletedResetPassword (Ok ( user, lang )) ->
            ( { model | transition = Just user }, Session.storeLogin lang user )

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

        UserSaved ->
            ( model, Nav.replaceUrl model.navKey "/" )


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
        { password, resetToken, session } =
            Validate.fromValid validModel
    in
    Session.requestResetPassword CompletedResetPassword { newPassword = password, token = resetToken } session



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
subscriptions _ =
    Session.userLoggedIn UserSaved
