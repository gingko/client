module Page.Signup exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , email : String
    , password : String
    , passwordConfirm : String
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session, email = "", password = "", passwordConfirm = "" }
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
            let
                requestBody =
                    Enc.object
                        [ ( "email", Enc.string model.email )
                        , ( "password", Enc.string model.password )
                        ]
                        |> Http.jsonBody

                responseDecoder =
                    Dec.field "name" Dec.string
            in
            ( model
            , Http.post
                { url = "/signup"
                , body = requestBody
                , expect = Http.expectJson CompletedSignup responseDecoder
                }
            )

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



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "form-page" ]
        [ div [ class "center-form" ]
            [ form [ onSubmit SubmittedForm ]
                [ label [] [ text "Email" ]
                , input
                    [ onInput EnteredEmail
                    , value model.email
                    , autofocus True
                    ]
                    []
                , label [] [ text "Password" ]
                , input
                    [ onInput EnteredPassword
                    , value model.password
                    ]
                    []
                , label [] [ text "Confirm Password" ]
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
