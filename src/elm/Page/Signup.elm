module Page.Signup exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Session exposing (Session)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifFalse, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { session : Session
    , email : String
    , password : String
    , passwordConfirm : String
    , errors : List ( Field, String )
    }


type Field
    = Email
    | Password
    | PasswordConfirm


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session, email = "", password = "", passwordConfirm = "", errors = [] }
    , Cmd.none
    )


toSession : Model -> Session
toSession { session } =
    session



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | EnteredPassConfirm String
    | CompletedSignup (Result Http.Error String)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        CompletedSignup (Ok email) ->
            ( model, Session.save email )

        CompletedSignup (Err error) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Nav.replaceUrl (Session.navKey session) "/" )


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
        model =
            Validate.fromValid validModel

        requestBody =
            Enc.object
                [ ( "email", Enc.string model.email )
                , ( "password", Enc.string model.password )
                ]
                |> Http.jsonBody

        responseDecoder =
            Dec.field "name" Dec.string
    in
    Http.post
        { url = "/signup"
        , body = requestBody
        , expect = Http.expectJson CompletedSignup responseDecoder
        }



-- VIEW


view : Model -> Html Msg
view model =
    let
        emailErrors =
            getFieldErrors Email model.errors

        passwordErrors =
            getFieldErrors Password model.errors

        passwordConfirmErrors =
            getFieldErrors PasswordConfirm model.errors
    in
    div [ id "form-page" ]
        [ div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ label [] [ text "Email" ]
                , div [] [ text (String.join "\n" emailErrors) ]
                , input
                    [ onInput EnteredEmail
                    , value model.email
                    , autofocus True
                    ]
                    []
                , label [] [ text "Password" ]
                , div [] [ text (String.join "\n" passwordErrors) ]
                , input
                    [ onInput EnteredPassword
                    , value model.password
                    ]
                    []
                , label [] [ text "Confirm Password" ]
                , div [] [ text (String.join "\n" passwordConfirmErrors) ]
                , input
                    [ onInput EnteredPassConfirm
                    , value model.passwordConfirm
                    ]
                    []
                , button [] [ text "Signup" ]
                , span [] [ text "or ", a [ href "/login" ] [ text "Login" ] ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
