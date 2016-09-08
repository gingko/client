port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Task


main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


-- Keep this here, to make sure Model can be passed through port
port saveCardChanges : Card -> Cmd msg
port scrollToActive : Int -> Cmd msg



-- MODEL


type alias Model =
  { cards : List Card
  , root : Int
  , active : Int
  , editing : Maybe Int
  , field : String
  , uid : Int
  }

type alias Card =
  { id : Int
  , content : String
  , children : List Int
  }

emptyModel : Model
emptyModel =
  { cards = [ Card 0 "Testing" [1,2,3]
            , Card 1 "test 2" []
            , Card 2 "test 3" []
            , Card 3 "test 4" []
            , Card 4 "test 5" []
            , Card 5 "test 6" []
            , Card 6 "test 6" []
            ]
  , root = 0
  , active = 1
  , editing = Nothing
  , field = ""
  , uid = 6
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! [ scrollToActive 1 ]



-- UPDATE


type Msg
    = NoOp
    | ActivateCard Int
    | OpenCard Int
    | UpdateField String
    | CancelCard
    | SaveContent Int String
    | InsertCardAfter (Maybe Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    ActivateCard id ->
      { model | active = id }
      ! [ scrollToActive id ]

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

    InsertCardAfter Nothing ->
      let
        newId = model.uid + 1
      in
        { model
          | cards = (Card newId "" []) :: model.cards
          , uid = newId
          , editing = Just newId
          , active = newId
        }
        ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) ( Dom.focus ( "card-edit-" ++ toString newId )) ]

    InsertCardAfter (Just id) ->
      model ! []



-- VIEW


view : Model -> Html Msg
view model =
  div
    [ id "column"
    , class "column" 
    ]
    [ div [class "buffer"][ ]
    , button [ onClick (InsertCardAfter Nothing) ][text "+"]
    , viewGroup model (getChildren model.cards model.root)
    , div [class "buffer"][ ]
    ]        


viewGroup : Model -> List Card -> Html Msg
viewGroup model cards =
  div
    [ class "group"
    ]
    ( List.map (viewCard model) cards )


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

getCard : List Card -> Int -> Maybe Card
getCard cards id =
  cards |> List.filter (\c -> c.id == id)
        |> List.head 


getCards : List Int -> List Card -> List Card
getCards ids cards =
  List.filterMap (getCard cards) ids


getChildren : List Card -> Int -> List Card
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
