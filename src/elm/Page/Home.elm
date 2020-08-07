module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Browser.Navigation as Nav
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import RandomId
import Session exposing (Session)
import Translation exposing (langFromString)



-- MODEL


type alias Model =
    { documents : List DocEntry
    , language : Translation.Language
    , session : Session
    }


type alias DocEntry =
    { name : String, state : DocState }


type DocState
    = Local


init : Session -> ( Model, Cmd msg )
init session =
    ( { documents = [], language = langFromString "en", session = session }
    , Cmd.none
    )


toSession : Model -> Session
toSession model =
    model.session



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
            ( model, Nav.pushUrl (Session.navKey model.session) docId )
