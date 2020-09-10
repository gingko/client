module Page.DocNew exposing (Model, Msg, init, update)

import RandomId
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    Session


init : Session -> ( Model, Cmd Msg )
init session =
    ( session, RandomId.generate NewDocIdReceived )



-- UPDATE


type Msg
    = NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDocIdReceived docId ->
            ( model, Route.replaceUrl (Session.navKey model) (Route.DocUntitled docId) )
