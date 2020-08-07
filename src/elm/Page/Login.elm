module Page.Login exposing (Model, Msg, init, toNavKey, update, view)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (placeholder, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Result exposing (Result)



-- MODEL


type alias Model =
    { navKey : Nav.Key, email : String, password : String }


init : Nav.Key -> ( Model, Cmd msg )
init navKey =
    ( { navKey = navKey, email = "", password = "" }
    , Cmd.none
    )



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
        ]



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result Http.Error String)


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
                , url = "http://localhost:5984/_session"
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
            ( model, Nav.replaceUrl model.navKey "/" )

        CompletedLogin (Err error) ->
            ( model, Cmd.none )



-- EXPORT


toNavKey : Model -> Nav.Key
toNavKey =
    .navKey
