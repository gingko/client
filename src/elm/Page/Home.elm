module Page.Home exposing (Model, Msg, init, toNavKey, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import RandomId
import Translation exposing (langFromString)



-- MODEL


type alias Model =
    { documents : List DocEntry
    , language : Translation.Language
    , navKey : Nav.Key
    }


type alias DocEntry =
    { name : String, state : DocState }


type DocState
    = Local


init : Nav.Key -> ( Model, Cmd msg )
init navKey =
    ( { documents = [], language = langFromString "en", navKey = navKey }
    , Cmd.none
    )


toNavKey : Model -> Nav.Key
toNavKey model =
    model.navKey



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "This is the home page"
        , button [ onClick GetNewDocId ] [ text "New" ]
        ]



-- UPDATE


type Msg
    = GetNewDocId
    | NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetNewDocId ->
            ( model, RandomId.generate NewDocIdReceived )

        NewDocIdReceived docId ->
            ( model, Nav.pushUrl model.navKey docId )
