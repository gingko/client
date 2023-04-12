port module Electron.TrialModal exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (href, id)
import Html.Events exposing (onClick)
import Translation exposing (Language(..))


main : Program ( Int, Int, String ) Model Msg
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
    , daysLeft : Int
    , limit : Int
    , purchaseURL : String
    }


init ( daysLeft, limit, purchaseURL ) =
    ( { language = En
      , daysLeft = daysLeft
      , limit = limit
      , purchaseURL = purchaseURL
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | ClickedEnter
    | ClickedContinue


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        ClickedEnter ->
            ( model, clicked "enter" )

        ClickedContinue ->
            ( model, clicked "continue" )



-- VIEW


view : Model -> List (Html Msg)
view { daysLeft, limit, purchaseURL } =
    [ div [ id "trial-body" ]
        [ h1 [] [ text "Gingko Writer - Free Trial" ]
        , div [] [ h3 [] [ text "You have ", span [] [ text <| String.fromInt (limit - daysLeft) ], text " days left in your free trial." ] ]
        , p [] [ text "I hope you're finding Gingko Writer useful." ]
        ]
    , div [ id "trial-buttons" ]
        [ a [ href purchaseURL ] [ text "Buy License Key..." ]
        , button [ onClick ClickedEnter ] [ text "Enter License Key" ]
        , button [ onClick ClickedContinue ] [ text "Continue Trial" ]
        ]
    ]


port clicked : String -> Cmd msg
