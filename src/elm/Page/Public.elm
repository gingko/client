module Page.Public exposing (..)

import Browser.Navigation as Nav
import Html exposing (Html, div, h1, p, text)



-- MODEL


type alias Model =
    { title : String
    , content : String
    , navKey : Nav.Key
    }


init : Nav.Key -> Model
init key =
    { title = "Public Page"
    , content = "This is a public page."
    , navKey = key
    }



-- GETTERS


navKey : Model -> Nav.Key
navKey model =
    model.navKey



-- UPDATE


type Msg
    = NoOp



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , p [] [ text model.content ]
        ]
