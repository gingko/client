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
port activateCard : CardCoords -> Cmd msg




-- MODEL


type alias Model =
  { cards : CardData
  , root : Id
  , uid : Id
  , active : CardCoords
  , editing : Maybe CardCoords
  }
  

-- Components

type alias Card =
  { id : Id
  , content : String
  , children : List Id
  }

type alias Group = List Card

type alias Column = List Group


-- Components (statically linked)

type alias CardInstance =
  { id : Id
  , content : String
  , children : Group
  , active : Bool
  , editing : Bool
  , field : String
  , coords : CardCoords
  }

type alias GroupInstance = List CardInstance

type alias ColumnInstance = List GroupInstance


-- Convenience types

type alias Id = Int
type alias CardData = List Card
type alias CardCoords = {column: Int, group: Int, card: Int}

emptyModel : Model
emptyModel =
  { cards = [ emptyCard ]
  , root = 0
  , uid = 0
  , active = { column = 0, group = 0, card = 0 }
  , editing = Nothing
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
    | ActivateCard CardCoords


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    ActivateCard _ ->
      model ! []

-- VIEW


view : Model -> Html Msg
view model =
  div
    [ ]
    [ viewStructure (buildTreeInstance model) ]


viewStructure : List ColumnInstance -> Html Msg
viewStructure cols =
  div
    [ id "app" ]
    (List.map viewColumn cols)


viewColumn : ColumnInstance -> Html Msg
viewColumn col =
  div
    [ class "column" ]
    (List.map viewGroup col)


viewGroup : GroupInstance -> Html Msg
viewGroup group =
  div
    [ class "group" ]
    (List.map viewCard group)


viewCard : CardInstance -> Html Msg
viewCard card =
  div
    [ classList [ ( "card", True )
                , ( "active", card.active )
                , ( "editing", card.editing )
                ]
    , id ( "card-" ++ toString card.id )
    , onClick (ActivateCard card.coords)
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


staticCard : Model -> CardCoords -> Card -> CardInstance
staticCard model coords card =
  let
    editState = case model.editing of
      Nothing ->
        False
      Just editCoords ->
        coords == editCoords
  in
    { id = card.id
    , content = card.content
    , children = getCards model.cards card.children
    , active = coords == model.active
    , editing = editState
    , field = card.content
    , coords = coords
    }


staticGroup : Model -> (Int, Int) -> Group -> GroupInstance
staticGroup model coords group =
  let
    indices = [0 .. List.length group]
    cardCoords =  
      indices
        |> List.map (\i -> { column = fst coords
                           , group = snd coords
                           , card = i
                           }) 
  in
    List.map2 (staticCard model) cardCoords group


staticColumn : Model -> Int -> Column -> ColumnInstance
staticColumn model cCoord col =
  let
    indices = [0 .. List.length col]
    gCoords =  
      indices
        |> List.map (\i -> (cCoord, i)) 
  in
    List.map2 (staticGroup model) gCoords col


buildTreeInstance : Model -> List ColumnInstance
buildTreeInstance model =
  let
    rootCard = model.root |> getCard model.cards
                          |> Maybe.withDefault emptyCard
                          |> staticCard model {column = 0, group = 0, card = 0}
  in
    [ [[rootCard]] ]


-- nextColumn : CardData -> Column -> Column
-- nextColumn cards col =
--   col |> List.concat -- List Card
--       |> List.map (staticCard cards) -- List CardInstance
--       |> List.map .children -- List Group == Column
-- 
-- 
-- buildColumns : CardData -> Id -> List Column
-- buildColumns cards rootId =
--   let
--     rootCard = rootId |> getCard cards
--                       |> Maybe.withDefault emptyCard
--   in
--     [ [[rootCard]]
--     , ( nextColumn cards [[rootCard]])
--     , ( nextColumn cards (nextColumn cards [[rootCard]]))
--     ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
