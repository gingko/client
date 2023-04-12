module Page.ForgotPassword exposing (Model, Msg, globalData, init, navKey, subscriptions, toUser, update, view)

import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Result exposing (Result)
import Route
import Session exposing (Session)
import Translation exposing (Language)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Session
    , email : String
    , errors : List ( Field, String )
    , sent : Bool
    , navKey : Nav.Key
    }


type Field
    = Form
    | Email


init : Nav.Key -> GlobalData -> Session -> Maybe String -> ( Model, Cmd msg )
init nKey gData session email_ =
    ( { globalData = gData
      , session = session
      , email = email_ |> Maybe.withDefault ""
      , errors = []
      , sent = False
      , navKey = nKey
      }
    , Cmd.none
    )


toUser : Model -> Session
toUser model =
    model.session


navKey : Model -> Nav.Key
navKey model =
    model.navKey


globalData : Model -> GlobalData
globalData model =
    model.globalData



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | CompletedForgotPassword (Result Http.Error ( Session, Language ))
    | GotUser Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate modelValidator model of
                Ok validModel ->
                    ( model
                    , sendForgotPasswordRequest validModel
                    )

                Err errs ->
                    ( { model | errors = errs }, Cmd.none )

        EnteredEmail email ->
            ( { model | email = email }, Cmd.none )

        CompletedForgotPassword (Ok _) ->
            ( { model | sent = True }, Cmd.none )

        CompletedForgotPassword (Err error) ->
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
                                404 ->
                                    ( Form, "Email not found.\n\nAre you trying to login with your gingkoapp.com username/password?\n\nYou'll need to create a new account to use the new version." )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ] }, Cmd.none )

        GotUser user ->
            ( { model | session = user }, Route.pushUrl model.navKey Route.Root )


modelValidator : Validator ( Field, String ) Model
modelValidator =
    Validate.all
        [ Validate.firstError
            [ ifBlank .email ( Email, "Please enter an email address." )
            , ifInvalidEmail .email (\_ -> ( Email, "This does not seem to be a valid email." ))
            ]
        ]


sendForgotPasswordRequest : Valid Model -> Cmd Msg
sendForgotPasswordRequest validModel =
    let
        { email, session } =
            Validate.fromValid validModel
    in
    Session.requestForgotPassword CompletedForgotPassword email session



-- VIEW


view : Model -> Html Msg
view model =
    let
        formErrors =
            getFieldErrors Form model.errors

        emailErrors =
            getFieldErrors Email model.errors
    in
    div [ id "form-page" ]
        ([ div [ class "brand" ]
            [ img [ id "logo", src "gingko-leaf-logo.svg" ] []
            , h1 [] [ text "Gingko" ]
            ]
         , div [ class "page-bg" ] []
         ]
            ++ (if not model.sent then
                    [ h1 [ class "headline" ] [ text "Password Reset" ]
                    , div [ class "header" ] [ span [ class "alt-action" ] [ text "New to Gingko? ", a [ href "/signup" ] [ text "Signup" ] ] ]
                    , div [ class "center-form" ]
                        [ form [ onSubmit SubmittedForm ]
                            [ div [ id "form-error" ] [ text (String.join "\n" formErrors) ]
                            , div [ class "input-error" ] [ text (String.join "\n" emailErrors) ]
                            , input
                                [ onInput EnteredEmail
                                , placeholder "Email"
                                , type_ "email"
                                , value model.email
                                , autofocus True
                                ]
                                []
                            , button [ class "cta" ] [ text "Send Reset Token" ]
                            ]
                        ]
                    ]

                else
                    [ h1 [ class "headline" ] [ text "Reset Email Sent" ]
                    , div [ class "center-form" ]
                        [ text "Check your email for your password reset link."
                        , br [] []
                        , br [] []
                        , small [] [ text "No email received? ", a [ href <| Route.toString (Route.ForgotPassword Nothing) ] [ text "Try again" ] ]
                        ]
                    ]
               )
        )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.loginChanges GotUser
