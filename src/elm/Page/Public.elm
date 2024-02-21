module Page.Public exposing (..)

import Api
import Browser.Navigation as Nav
import Html exposing (Html, div, h1, p, text)
import Http
import Json.Decode as Dec



-- MODEL


type alias Model =
    { title : String
    , content : String
    , navKey : Nav.Key
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key dbName =
    ( { title = "Public Page"
      , content = "This is a public page."
      , navKey = key
      }
    , getPublicDocument dbName
    )



-- GETTERS


navKey : Model -> Nav.Key
navKey model =
    model.navKey



-- UPDATE


type Msg
    = NoOp
    | DataReceived (Result Http.Error Dec.Value)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.title ]
        , p [] [ text model.content ]
        ]



-- REQUESTS


getPublicDocument : String -> Cmd Msg
getPublicDocument dbName =
    Api.getPublicDocument DataReceived dbName
