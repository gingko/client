module Page.Signup exposing (Model, Msg, init, toSession, update, view)

import Html exposing (..)
import Html.Attributes exposing (href, placeholder, value)
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
                { url = "http://localhost:3000/signup"
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
        , fieldset []
            [ input
                [ placeholder "Confirm Password"
                , onInput EnteredPassConfirm
                , value model.passwordConfirm
                ]
                []
            ]
        , button [] [ text "Signup" ]
        , a [ href "/login" ] [ text "Login" ]
        ]
