module Page.Login exposing (Model, Msg, init, subscriptions, toUser, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, src, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Json.Decode as Dec
import Json.Encode as Enc
import Result exposing (Result)
import Route
import User exposing (User)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { user : User, email : String, password : String, errors : List ( Field, String ) }


type Field
    = Form
    | Email
    | Password


init : User -> ( Model, Cmd msg )
init user =
    ( { user = user, email = "", password = "", errors = [] }
    , Cmd.none
    )


toUser : Model -> User
toUser model =
    model.user



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error String)
    | GotUser User


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate modelValidator model of
                Ok validModel ->
                    ( model
                    , sendLoginRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        CompletedLogin (Ok email) ->
            ( model, User.save email )

        CompletedLogin (Err error) ->
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
                                401 ->
                                    ( Form, "Email or Password was incorrect. Please try again." )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ], password = "" }, Cmd.none )

        GotUser user ->
            ( { model | user = user }, Route.pushUrl (User.navKey user) Route.Home )


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Please enter an email address." )
            , ifInvalidEmail .email (\_ -> ( Email, "This does not seem to be a valid email." ))
            ]
        , ifBlank .password ( Password, "Please enter a password." )
        ]


sendLoginRequest : Valid Model -> Cmd Msg
sendLoginRequest validModel =
    let
        model =
            Validate.fromValid validModel

        requestBody =
            Enc.object
                [ ( "name", Enc.string model.email )
                , ( "password", Enc.string model.password )
                ]
                |> Http.jsonBody

        responseDecoder =
            Dec.field "name" Dec.string
    in
    Http.riskyRequest
        { method = "POST"
        , url = "/db/_session"
        , headers = []
        , body = requestBody
        , expect = Http.expectJson CompletedLogin responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



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
    in
    div [ id "form-page" ]
        [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
        , div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ div [] [ text (String.join "\n" formErrors) ]
                , label [] [ text "Email" ]
                , div [] [ text (String.join "\n" emailErrors) ]
                , input
                    [ onInput EnteredEmail
                    , value model.email
                    ]
                    []
                , label [] [ text "Password" ]
                , div [] [ text (String.join "\n" passwordErrors) ]
                , input
                    [ onInput EnteredPassword
                    , value model.password
                    ]
                    []
                , button [] [ text "Login" ]
                , span [ class "alt-action" ] [ text "or ", a [ href "/signup" ] [ text "Signup" ] ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    User.changes GotUser (User.navKey model.user)
