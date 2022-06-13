module Electron.TrialModal exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (id)
import Translation exposing (Language(..))


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                Browser.Document "Free Trial"
                    (view m)
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { language : Language
    }


defaultModel =
    { language = En
    }


init () =
    ( defaultModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view m =
    [ div [ id "trial-body" ]
        [ h1 [] [ text "Gingko Writer - Free Trial" ]
        , div [] [ h3 [] [ text "You have ", span [] [ text "9999" ], text " days left in your free trial." ] ]
        , p [] [ text "I hope you're finding Gingko Writer useful." ]
        ]
    , div [ id "trial-buttons" ]
        [ button [] [ text "Buy License Key..." ]
        , button [] [ text "Enter License Key" ]
        , button [] [ text "Continue Trial" ]
        ]
    ]
