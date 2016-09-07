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


-- Keep this here, to make sure Model can be passed through port
port setStorage : Model -> Cmd msg



-- MODEL


type alias Model =
  { cards : List Card
  , editing : Maybe Int
  , editField : String
  , uid : Int
  }

type alias Card =
  { content : String
  , id : Int
  }

emptyModel : Model
emptyModel =
  { cards = [newCard "Model card test" 0 ]
  , editing = Nothing
  , editField = ""
  , uid = 1
  }


newCard : String -> Int -> Card
newCard cont id =
  { content = cont
  , id = id
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


type Msg
    = NoOp
    | EditingCard Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    EditingCard eid ->
      { model | editing = Just eid }
        ! []




-- VIEW


view : Model -> Html Msg
view model =
  div [ ] [viewCard (Maybe.withDefault (newCard "default" 0 ) (List.head model.cards)) ]
        
    


viewCard : Card -> Html Msg
viewCard card =
  div
    [ class "card"
    ]
    [ text card.content ]
