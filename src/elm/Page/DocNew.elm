module Page.DocNew exposing (Model, Msg, init, update)

import RandomId
import Route
import User exposing (User)



-- MODEL


type alias Model =
    User


init : User -> ( Model, Cmd Msg )
init session =
    ( session, RandomId.generate NewDocIdReceived )



-- UPDATE


type Msg
    = NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewDocIdReceived docId ->
            ( model, Route.replaceUrl (User.navKey model) (Route.DocUntitled docId) )
