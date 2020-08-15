module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (href, placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Result exposing (Result)
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session, email : String, password : String }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session, email = "", password = "" }
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
            let
                requestBody =
                    Enc.object
                        [ ( "name", Enc.string model.email )
                        , ( "password", Enc.string model.password )
                        ]
                        |> Http.jsonBody

                responseDecoder =
                    Dec.field "name" Dec.string
            in
            ( model
            , Http.riskyRequest
                { method = "POST"
                , url = "/db/_session"
                , headers = []
                , body = requestBody
                , expect = Http.expectJson CompletedLogin responseDecoder
                , timeout = Nothing
                , tracker = Nothing
                }
            )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        EnteredPassword password ->
            ( { model | password = password }, Cmd.none )

        CompletedLogin (Ok email) ->
            ( model, Session.save email )

        CompletedLogin (Err error) ->
            ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }, Nav.replaceUrl (Session.navKey session) "/" )



-- VIEW


view : Model -> Html Msg
view model =
    form [ onSubmit SubmittedForm ]
        [ fieldset []
            [ input
                [ placeholder "Email"
                , onInput EnteredEmail
                , value model.email
                ]
                []
            ]
        , fieldset []
            [ input
                [ placeholder "Password"
                , onInput EnteredPassword
                , value model.password
                ]
                []
            ]
        , button [] [ text "Login" ]
        , a [ href "/signup" ] [ text "Signup" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)
