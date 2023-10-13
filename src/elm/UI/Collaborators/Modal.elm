module UI.Collaborators.Modal exposing (..)

-- MODEL

import Html exposing (Html, div, text)
import Translation


type alias Model =
    ()


init : Model
init =
    ()



-- VIEW


view : Translation.Language -> Model -> List (Html msg)
view lang model =
    [ div [] [ text "Collaborators Modal" ]
    ]
