module UI.Collaborators.Modal exposing (..)

-- MODEL

import Ant.Icons.Svg as AntIcon
import Html exposing (Html, br, div, strong, text)
import Html.Attributes exposing (class)
import Svg.Attributes exposing (height, width)
import Translation


type alias Model =
    ()


init : Model
init =
    ()



-- VIEW


view : Translation.Language -> Model -> List (Html msg)
view lang model =
    [ div
        [ class "bg-amber-100"
        , class "border-amber-400"
        , class "rounded-md"
        , class "flex"
        , class "gap-4"
        , class "p-4"
        , class "text-amber-900"
        , class "fill-orange-600"
        , class "items-center"
        , class "mb-4"
        ]
        [ AntIcon.warningFilled [ width "30px", height "30px" ]
        , div []
            [ text "Realtime Collaboration is "
            , strong [] [ text "currently in beta" ]
            , text "."
            , br [] []
            , text "Please back up your document regularly while testing this feature."
            ]
        ]
    , div [] [ text "Collaborators Modal" ]
    ]
