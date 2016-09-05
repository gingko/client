port module Main exposing (..)


import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)


main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model =
  let
    (newModel, cmds) =
      update msg model
  in
    ( newModel
    , Cmd.batch [ setStorage newModel, cmds ]
    )


-- MODEL


-- The full application state of our Gingko app.
-- (single tree for now)
type alias Model =
  { root: Int
  , cards : List Card
  , uid : Int
  }

type alias Card =
  { content : String
  , cardType : String
  , children : List Int
  , id : Int
  }

emptyModel : Model
emptyModel =
  { root = 0
  , cards = [newCard "test" 0]
  , uid = 1
  }


newCard : String -> Int -> Card
newCard cont id =
  { content = cont
  , cardType = "text/markdown"
  , children = []
  , id = id
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
-- VIEW


view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper"
    , style [ ("visibility", "hidden") ]
    ]
    [ section
        [ class "todoapp" ]
        [ text "test"
        ]
    ]
