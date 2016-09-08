port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra exposing (find, interweave)
import Task


main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


port saveCardChanges : Card -> Cmd msg
port activateCard : (Int, Int, Int) -> Cmd msg




-- MODEL


type alias Model =
  { cards : CardData
  , root : Id
  , uid : Id
  , transient : ViewState
  }
  

type alias Column = List Group

type alias Group = List Card

type alias Card =
  { id : Id
  , content : String
  , children : List Id
  , active : Bool
  , editing : Bool
  , field : String
  }

type alias ViewState =
  { active : (Int, Int, Int)
  , editing : (Int, Int, Int)
  }

type alias Id = Int
type alias CardData = List Card

emptyModel : Model
emptyModel =
  { cards = [ emptyCard ]
  , root = 0
  , uid = 0
  , transient = { active = (0,0,0), editing = (0,0,0) }
  }

emptyCard : Card
emptyCard =
  Card 0 "" [] False False ""

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  let
    selectedModel = Maybe.withDefault emptyModel savedModel
  in
    selectedModel ! [ ]



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
    [ ]
    [ viewCard ( Maybe.withDefault emptyCard (getCard model.cards model.root) ) ]        


viewCard : Card -> Html Msg
viewCard card =
  div
    [ classList [ ( "card", True )
                , ( "active", card.active )
                , ( "editing", card.editing )
                ]
    , id ( "card-" ++ toString card.id )
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ id ( "card-edit-" ++ toString card.id )
            , class "edit"
            , value card.field
            ]
            []
    ]


-- HELPERS


getCard : CardData -> Id -> Maybe Card
getCard cards id =
  List.Extra.find (\c -> c.id == id) cards


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
