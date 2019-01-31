module Fonts exposing (Model, Msg, content, default, heading, monospace, setSystem, update, viewSelector)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Html.Events.Extra exposing (targetValueMaybe)
import Json.Decode as Json



-- MODEL


type Model
    = Model
        { heading : String
        , content : String
        , monospace : String
        , builtin : List String
        , system : List String
        }


default : Model
default =
    Model
        { heading = "Bitter"
        , content = "Open Sans"
        , monospace = "Droid Sans Mono"
        , builtin = [ "Bitter", "Open Sans", "Droid Sans Mono" ]
        , system = []
        }


setSystem : List String -> Model -> Model
setSystem sysFonts (Model ({ system } as model)) =
    Model
        { model | system = system ++ sysFonts |> List.sort }


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
viewSelector (Model { heading, content, monospace, builtin, system }) =
    let
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

        fontList sel =
            let
                optionFunction f =
                    option [ style [ ( "font-family", f ) ], value f, selected (sel == f) ] [ text f |> Debug.log "option text" ]
            in
            {- This would be better, but leads to random font indents within each group for some reason
               [ optgroup [ attribute "label" "Gingko Built-in", style [ ( "font-family", "Bitter" ) ] ] (List.map optionFunction builtin)
               , optgroup [ attribute "label" "System Installed", style [ ( "font-family", "Bitter" ) ] ] (List.map optionFunction system)
               ]
            -}
            List.map optionFunction builtin ++ [ option [ disabled True ] [ text "───────────────" ] ] ++ List.map optionFunction system

        onSelect msgFunction =
            on "change" (Json.map msgFunction targetValueMaybe)
    in
    div
        [ class "horizontal-dialog" ]
        [ div []
            [ span [ style [ ( "font-family", heading ), ( "font-weight", "bold" ) ] ] [ text "heading Font" ]
            , br [] []
            , select [ onSelect headingFunction ] (fontList heading)
            ]
        , div []
            [ span [ style [ ( "font-family", content ) ] ] [ text "Content Font" ]
            , br [] []
            , select [ onSelect contentFunction ] (fontList content)
            ]
        , div []
            [ span [ style [ ( "font-family", monospace ) ] ] [ text "Editing/Monospace Font" ]
            , br [] []
            , select [ onSelect monospaceFunction ] (fontList monospace)
            ]
        , button [ onClick CloseSelector ] [ text "OK" ]
        ]
