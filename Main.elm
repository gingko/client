port module Main exposing (..)


import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)


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
  { card : Card
  }

type alias Card =
  { content : String
  , editing : Bool
  }

emptyModel : Model
emptyModel =
  { card = Card "Testing" False
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []



-- UPDATE


type Msg
    = NoOp
    | ToggleEdit


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []
    ToggleEdit ->
      { model | card = Card model.card.content ( not model.card.editing ) }
        ! []




-- VIEW


view : Model -> Html Msg
view model =
  div [ ] [ viewCard model.card ]
        
    


viewCard : Card -> Html Msg
viewCard card =
  div
    [ classList [( "card", True ), ( "editing", card.editing )]
    , onDoubleClick ToggleEdit
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ class "edit"
            , value card.content
            , onBlur ToggleEdit
            ]
            []
    ]
