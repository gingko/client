module Page.DocNew exposing (Model, Msg, init, update)

import Browser.Navigation as Nav
import GlobalData exposing (GlobalData)
import RandomId
import Route
import Session exposing (Session)



-- MODEL


type alias Model =
    { globalData : GlobalData
    , session : Session
    , navKey : Nav.Key
    }


init : Nav.Key -> GlobalData -> Session -> ( Model, Cmd Msg )
init navKey gData session =
    ( Model gData session navKey, RandomId.generate NewDocIdReceived )



-- UPDATE


type Msg
    = NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDocIdReceived docId ->
            ( model, Route.replaceUrl model.navKey (Route.DocUntitled docId) )
