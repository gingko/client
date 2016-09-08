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
port activateCard : Id -> Cmd msg




-- MODEL


type alias Model =
  { cards : List Card
  , root : Id
  , active : Id
  , editing : Maybe Id
  , field : String
  , uid : Id
  }

type alias Card =
  { id : Id
  , content : String
  , children : List Id
  }

type alias Id = Int

emptyModel : Model
emptyModel =
  { cards = [ Card 0 "Testing" []
            ]
  , root = 0
  , active = 0
  , editing = Nothing
  , field = ""
  , uid = 0
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  let
    selectedModel = Maybe.withDefault emptyModel savedModel
  in
    selectedModel ! [ activateCard selectedModel.active ]



-- UPDATE


type Msg
    = NoOp
    | ActivateCard Id
    | OpenCard Id
    | UpdateField String
    | CancelCard
    | SaveContent Id String
    | InsertAt Id Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    ActivateCard id ->
      { model | active = id }
      ! [ activateCard id ]

    OpenCard id ->
      { model
        | editing = Just id
        , field = model.cards |> List.filter (\c -> c.id == id)
                                |> List.head
                                |> Maybe.withDefault (Card 0 "" [])
                                |> .content
      }
        ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) ( Dom.focus ( "card-edit-" ++ toString id )) ]

    UpdateField str ->
      { model | field = str }
        ! []

    CancelCard ->
      { model | editing = Nothing } ! []

    SaveContent id str ->
      let
        updateCard c =
          if c.id == id then { c | content = str } else c
      in
        { model 
          | cards = List.map updateCard model.cards
          , editing = Nothing
          , field = ""
        } 
          ! [ saveCardChanges (Card id str []) ]

    InsertAt parentId index ->
      let
        newId = model.uid + 1
        newCard = Card newId "" []
        updateParent c =
          if c.id == parentId then
             { c | children = (List.take index c.children) ++ [newId] ++ (List.drop index c.children) }
          else c
        newList = (List.map updateParent model.cards) ++ [newCard]
        changedParent = newList |> List.Extra.find (\c -> c.id == parentId)
                                |> Maybe.withDefault (Card parentId "" [newId])
          
      in
        { model
          | cards = newList
          , editing = Just newId
          , field = ""
          , active = newId
          , uid = newId
        } 
          ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) ( Dom.focus ( "card-edit-" ++ toString newId ))
            , saveCardChanges changedParent
            ]




-- VIEW


view : Model -> Html Msg
view model =
  div
    [ id "column"
    , class "column" 
    ]
    [ div [class "buffer"][ ]
    , viewGroup model (getChildren model.cards model.root)
    , div [class "buffer"][ ]
    ]        


viewGroup : Model -> List Card -> Html Msg
viewGroup model cards =
  let
    indices = [0 .. (List.length cards)]
  in
    div
      [ class "group"
      ]
      (interweave ( List.map (viewSplit model.root) indices )
                  ( List.map (viewCard model) cards ))


viewSplit : Id -> Int -> Html Msg
viewSplit parentId index =
  div
    [ class "split"
    ]
    [ button [ onClick (InsertAt parentId index) ][text "+"]
    ]


viewCard : Model -> Card -> Html Msg
viewCard model card =
  div
    [ classList [ ( "card", True )
                , ( "active", (card.id == model.active ) )
                , ( "editing", (Just card.id == model.editing ) )
                ]
    , id ( "card-" ++ toString card.id )
    , onClick ( ActivateCard card.id )
    , onDoubleClick ( OpenCard card.id )
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ id ( "card-edit-" ++ toString card.id )
            , class "edit"
            , value model.field
            , onInput UpdateField
            , onBlur CancelCard
            , onEnter (SaveContent card.id model.field)
            ]
            []
    ]


-- HELPERS

getCard : List Card -> Id -> Maybe Card
getCard cards id =
  cards |> List.filter (\c -> c.id == id)
        |> List.head 


getCards : List Id -> List Card -> List Card
getCards ids cards =
  List.filterMap (getCard cards) ids


getChildren : List Card -> Id -> List Card
getChildren cards id =
  let
    root = getCard cards id
  in
    getCards (Maybe.withDefault (Card 0 "" []) root).children cards


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
