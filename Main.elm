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
  }

type alias StaticCard =
  { id : Id
  , content : String
  , children : Group
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
  Card 0 "" []

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
    [ viewStructure (buildColumns model.cards model.root) ]


viewStructure : List Column -> Html Msg
viewStructure cols =
  div
    [ id "app" ]
    (List.map viewColumn cols)


viewColumn : Column -> Html Msg
viewColumn col =
  div
    [ class "column" ]
    (List.map viewGroup col)


viewGroup : Group -> Html Msg
viewGroup group =
  let
    groupStatic = List.map (staticLinker group) group
  in
    div
      [ class "group" ]
      (List.map viewCard groupStatic)


viewCard : StaticCard -> Html Msg
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


getCards : CardData -> List Id -> Group
getCards cards ids =
  List.filterMap (getCard cards) ids


staticLinker : CardData -> Card -> StaticCard
staticLinker cards card =
  { id = card.id
  , content = card.content
  , children = getCards cards card.children
  , active = False
  , editing = False
  , field = ""
  }


nextColumn : CardData -> Column -> Column
nextColumn cards col =
  col |> List.concat -- List Card
      |> List.map (staticLinker cards) -- List StaticCard
      |> List.map .children -- List Group == Column


buildColumns : CardData -> Id -> List Column
buildColumns cards rootId =
  let
    rootCard = Maybe.withDefault emptyCard (getCard cards rootId)
  in
    [ [[rootCard]]
    , ( nextColumn cards [[rootCard]])
    , ( nextColumn cards (nextColumn cards [[rootCard]]))
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
