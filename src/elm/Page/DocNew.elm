module Page.DocNew exposing (Model, Msg, init, update)

import Browser.Navigation as Nav
import RandomId
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { session : Session
    , navKey : Nav.Key
    }


init : Nav.Key -> Session -> ( Model, Cmd Msg )
init navKey session =
    ( Model session navKey, RandomId.generate NewDocIdReceived )



-- UPDATE


type Msg
    = NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDocIdReceived docId ->
            ( model, Route.replaceUrl model.navKey (Route.DocUntitled docId) )
