module Page.Home exposing (Model, init, view)

import Html exposing (Html, div, text)
import Translation exposing (langFromString)



-- MODEL


type alias Model =
    { documents : List DocEntry, language : Translation.Language }


type alias DocEntry =
    { name : String, state : DocState }


type DocState
    = Local


init : Model
init =
    { documents = [], language = langFromString "en" }



-- VIEW


view : Model -> Html msg
view model =
    div [] [ text "This is the home page" ]
