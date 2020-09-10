module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (class, href, id, placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Result exposing (Result)
import Route
import Session exposing (Session)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, ifNotInt, validate)



-- MODEL


type alias Model =
    { session : Session, email : String, password : String, errors : List ( Field, String ) }


type Field
    = Email
    | Password


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session, email = "", password = "", errors = [] }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error String)
    | GotSession Session


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
            ( model, Session.save email )

        CompletedLogin (Err error) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Route.pushUrl (Session.navKey session) Route.Home )


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
        emailErrors : List String
        emailErrors =
            model.errors
                |> List.filter ((==) Email << Tuple.first)
                |> List.map Tuple.second
    in
    div [ id "form-page" ]
        [ div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ label [] [ text "Email" ]
                , div [] [ text (String.join "\n" emailErrors) ]
                , input
                    [ onInput EnteredEmail
                    , value model.email
                    ]
                    []
                , label [] [ text "Password" ]
                , input
                    [ onInput EnteredPassword
                    , value model.password
                    ]
                    []
                , button [] [ text "Login" ]
                , a [ href "/signup" ] [ text "Signup" ]
                ]
            ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
