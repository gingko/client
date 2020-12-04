module Page.ForgotPassword exposing (Model, Msg, init, subscriptions, toUser, update, view)

import Html exposing (..)
import Html.Attributes exposing (autofocus, class, href, id, placeholder, src, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http exposing (Error(..))
import Result exposing (Result)
import Route
import User exposing (User)
import Utils exposing (getFieldErrors)
import Validate exposing (Valid, Validator, ifBlank, ifInvalidEmail, validate)



-- MODEL


type alias Model =
    { user : User
    , email : String
    , errors : List ( Field, String )
    , sent : Bool
    }


type Field
    = Form
    | Email


init : User -> Maybe String -> ( Model, Cmd msg )
init user email_ =
    ( { user = user
      , email = email_ |> Maybe.withDefault ""
      , errors = []
      , sent = False
      }
    , Cmd.none
    )


toUser : Model -> User
toUser model =
    model.user



-- UPDATE


type Msg
    = SubmittedForm
    | EnteredEmail String
    | CompletedForgotPassword (Result Http.Error User)
    | GotUser User


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
                                401 ->
                                    ( Form, "Email or Password was incorrect. Please try again." )

                                _ ->
                                    fallbackMsg

                        _ ->
                            fallbackMsg
            in
            ( { model | errors = [ errorMsg ] }, Cmd.none )

        GotUser user ->
            ( { model | user = user }, Route.pushUrl (User.navKey user) Route.Root )


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
        { email, user } =
            Validate.fromValid validModel
    in
    User.requestForgotPassword CompletedForgotPassword email user



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
                            [ div [] [ text (String.join "\n" formErrors) ]
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
    User.loginChanges GotUser (User.navKey model.user)
