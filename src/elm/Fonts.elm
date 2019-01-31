module Fonts exposing (Model, Msg, content, default, heading, monospace, setSystem, update, viewSelector)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Html.Events.Extra exposing (targetValueMaybe)
import Json.Decode as Json



-- MODEL


type Model
    = Model { heading : String, content : String, monospace : String, fontList : List String }


default : Model
default =
    Model
        { heading = "Bitter"
        , content = "Open Sans"
        , monospace = "Droid Sans Mono"
        , fontList = [ "Bitter", "Open Sans", "Droid Sans Mono" ]
        }


setSystem : List String -> Model -> Model
setSystem sysFonts (Model ({ heading, content, monospace, fontList } as model)) =
    Model
        { model | fontList = fontList ++ List.sort sysFonts }


heading : Model -> String
heading (Model { heading }) =
    heading


content : Model -> String
content (Model { content }) =
    content


monospace : Model -> String
monospace (Model { monospace }) =
    monospace



-- UPDATE


type Msg
    = NoOp
    | ChangeHeading String
    | ChangeContent String
    | ChangeMonospace String
    | CloseSelector


update : Msg -> Model -> ( Model, Bool )
update msg (Model model) =
    case msg of
        ChangeHeading h ->
            ( Model { model | heading = h }, True )

        ChangeContent c ->
            ( Model { model | content = c }, True )

        ChangeMonospace m ->
            ( Model { model | monospace = m }, True )

        CloseSelector ->
            ( Model model, False )

        NoOp ->
            ( Model model, True )



-- VIEW


viewSelector : Model -> Html Msg
viewSelector (Model { heading, content, monospace, fontList }) =
    let
        optionFunction sel f =
            option [ style [ ( "font-family", f ) ], value f, selected (sel == f) ] [ text f ]

        headingFunction f_ =
            case f_ of
                Just f ->
                    ChangeHeading f

                Nothing ->
                    NoOp

        contentFunction f_ =
            case f_ of
                Just f ->
                    ChangeContent f

                Nothing ->
                    NoOp

        monospaceFunction f_ =
            case f_ of
                Just f ->
                    ChangeMonospace f

                Nothing ->
                    NoOp

        onSelect msgFunction =
            on "change" (Json.map msgFunction targetValueMaybe)
    in
    div
        [ class "horizontal-dialog" ]
        [ div []
            [ span [ style [ ( "font-family", heading ), ( "font-weight", "bold" ) ] ] [ text "heading Font" ]
            , br [] []
            , select [ style [ ( "font-family", heading ) ], onSelect headingFunction ] (List.map (optionFunction heading) fontList)
            ]
        , div []
            [ span [ style [ ( "font-family", content ) ] ] [ text "Content Font" ]
            , br [] []
            , select [ style [ ( "font-family", content ) ], onSelect contentFunction ] (List.map (optionFunction content) fontList)
            ]
        , div []
            [ span [ style [ ( "font-family", monospace ) ] ] [ text "Editing/Monospace Font" ]
            , br [] []
            , select [ style [ ( "font-family", monospace ) ], onSelect monospaceFunction ] (List.map (optionFunction monospace) fontList)
            ]
        , button [ onClick CloseSelector ] [ text "OK" ]
        ]
