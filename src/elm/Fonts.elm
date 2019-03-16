module Fonts exposing (Model, Msg, Settings, default, getContent, getHeading, getMonospace, init, setSystem, update, viewSelector)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Html.Events.Extra exposing (targetValueMaybe)
import Json.Decode as Json
import List.Extra exposing (unique)
import Translation exposing (Language, TranslationId(..), tr)



-- MODEL


type Model
    = Model
        { heading : String
        , content : String
        , monospace : String
        , builtin : List String
        , system : List String
        }


type alias Settings =
    { heading : String
    , content : String
    , monospace : String
    }


init : Maybe ( String, String, String ) -> Model
init f_ =
    case ( f_, default ) of
        ( Just ( headingIn, contentIn, monospaceIn ), Model defaultModel ) ->
            Model { defaultModel | heading = headingIn, content = contentIn, monospace = monospaceIn }

        ( Nothing, _ ) ->
            default


default : Model
default =
    Model
        { heading = "Bitter"
        , content = "Open Sans"
        , monospace = "Droid Sans Mono"
        , builtin =
            [ "Bitter"
            , "Open Sans"
            , "Droid Sans Mono"
            , "Merriweather"
            , "Libre Baskerville"
            , "Eczar"
            , "Open Dyslexic"
            ]
        , system = []
        }


setSystem : List String -> Model -> Model
setSystem sysFonts (Model ({ system } as model)) =
    Model
        { model | system = system ++ sysFonts |> unique |> List.sort }


getHeading : Model -> String
getHeading (Model { heading }) =
    heading


getContent : Model -> String
getContent (Model { content }) =
    content


getMonospace : Model -> String
getMonospace (Model { monospace }) =
    monospace



-- UPDATE


type Msg
    = NoOp
    | ChangeHeading String
    | ChangeContent String
    | ChangeMonospace String
    | CloseSelector


update : Msg -> Model -> ( Model, Bool, Maybe Settings )
update msg (Model model) =
    case msg of
        ChangeHeading h ->
            ( Model { model | heading = h }, True, Nothing )

        ChangeContent c ->
            ( Model { model | content = c }, True, Nothing )

        ChangeMonospace m ->
            ( Model { model | monospace = m }, True, Nothing )

        CloseSelector ->
            ( Model model, False, Just (model |> modelToSettings) )

        NoOp ->
            ( Model model, True, Nothing )


modelToSettings : { a | heading : String, content : String, monospace : String } -> Settings
modelToSettings { heading, content, monospace } =
    { heading = heading, content = content, monospace = monospace }



-- VIEW


viewSelector : Language -> Model -> Html Msg
viewSelector lang (Model { heading, content, monospace, builtin, system }) =
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
                    option [ style "font-family" f, value f, selected (sel == f) ] [ text f ]
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
        [ id "font-selector", class "horizontal-dialog" ]
        [ div []
            [ span [ style "font-family" heading, style "font-weight" "bold" ] [ text <| tr lang HeadingFont ]
            , br [] []
            , select [ onSelect headingFunction ] (fontList heading)
            ]
        , div []
            [ span [ style "font-family" content ] [ text <| tr lang ContentFont ]
            , br [] []
            , select [ onSelect contentFunction ] (fontList content)
            ]
        , div []
            [ span [ style "font-family" monospace ] [ text <| tr lang EditingFont ]
            , br [] []
            , select [ onSelect monospaceFunction ] (fontList monospace)
            ]
        , button [ onClick CloseSelector ] [ text "OK" ]
        ]
