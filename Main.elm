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
  , active : Int
  , editing : Maybe Int
  }

type alias Card =
  { content : String
  , id : Int
  }

emptyModel : Model
emptyModel =
  { cards = [ Card "Testing" 0
            , Card "test 2" 1
            , Card "test 3" 2
            , Card "test 4" 3
            , Card "test 5" 4
            , Card "test 6" 5
            , Card "test 6" 6
            ]
  , active = 0
  , editing = Nothing
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! [ scrollToActive 0 ]



-- UPDATE


type Msg
    = NoOp
    | ActivateCard Int
    | EditCard (Maybe Int)
    | UpdateCard Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    ActivateCard id ->
      { model | active = id }
      ! [ scrollToActive id ]
    EditCard Nothing ->
      { model | editing = Nothing } ! [ ]
    EditCard (Just id) ->
      { model | editing = Just id }
      ! [ Task.perform (\_ -> NoOp) (\_ -> NoOp) ( Dom.focus ( "card-edit-" ++ toString id )) ]
    UpdateCard id str ->
      let
        updateCard c =
          if c.id == id then { c | content = str } else c
      in
        { model | cards = List.map updateCard model.cards } 
        ! [ saveCardChanges (Card str id) ]




-- VIEW


view : Model -> Html Msg
view model =
  div
    [ id "column"
    , class "column" 
    ]
    [ div [class "buffer"][ ]
    , viewGroup model model.cards
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
    , onDoubleClick ( EditCard (Just card.id) )
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ id ( "card-edit-" ++ toString card.id )
            , class "edit"
            , value card.content
            , onInput (UpdateCard card.id)
            , onBlur (EditCard Nothing)
            , onEnter (EditCard Nothing)
            ]
            []
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
