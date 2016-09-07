port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
port setStorage : Model -> Cmd msg



-- MODEL


type alias Model =
  { cards : List Card
  }

type alias Card =
  { content : String
  , editing : Bool
  , id : Int
  }

emptyModel : Model
emptyModel =
  { cards = [ Card "Testing" False 0, Card "test 2" False 1 ]
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


type Msg
    = NoOp
    | EditCard Int Bool
    | UpdateCard Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    EditCard id bool ->
      let
        updateCard c =
          if c.id == id then { c | editing = bool } else c
      in
        { model | cards = List.map updateCard model.cards }
          ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) (Dom.focus "card-1") ]
    UpdateCard id str ->
      let
        updateCard c =
          if c.id == id then { c | content = str } else c
      in
        { model | cards = List.map updateCard model.cards } ! []




-- VIEW


view : Model -> Html Msg
view model =
  div [ ] ( List.map viewCard model.cards )
        
    


viewCard : Card -> Html Msg
viewCard card =
  div
    [ classList [( "card", True ), ( "editing", card.editing )]
    , onDoubleClick (EditCard card.id True)
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ id "card-1"
            , class "edit"
            , value card.content
            , onInput (UpdateCard card.id)
            , onBlur (EditCard card.id False)
            ]
            []
    ]
