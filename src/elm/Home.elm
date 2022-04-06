port module Home exposing (main)

import Browser
import Html exposing (Html, button, h1, text)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.document
        { init = always ( 0, Cmd.none )
        , update = update
        , view = always <| Browser.Document "Gingko Writer - Home" (view 0)
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    Int



-- UPDATE


type Msg
    = ClickedNew
    | ClickedOpen


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedNew ->
            ( model, send "ClickedNew" )

        ClickedOpen ->
            ( model, send "ClickedOpen" )



-- VIEW


view : Model -> List (Html Msg)
view model =
    [ h1 [] [ text "Here I AM!!!" ]
    , button [ onClick ClickedNew ] [ text "New Gingko Document" ]
    , button [ onClick ClickedOpen ] [ text "Open Gingko Document" ]
    ]



-- PORTS


port send : String -> Cmd msg
