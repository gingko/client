port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra exposing (find, interweave)
import Task


main : Program (Maybe Data)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

-- MODEL


type alias Model =
  { structure : List Column 
  , active : Coords
  , editing : Maybe Coords
  }
  
type alias Data =
  { cards : List Card
  , trees : List Tree
  , root : Id
  , active : Coords
  , editing : Maybe Coords
  }

type alias Card =
  { id : Id
  , tipo : String
  , content : String
  }

type alias Tree =
  { id : Id
  , card : Id
  , children : List Id
  }


type alias Group = List Card

type alias Column = List Group


-- Convenience types

type alias Id = Int
type alias Coords = (Int, Int, Int)

emptyModel : Model
emptyModel =
  { structure = [ [ [ emptyCard ] ] ]
  , active = (0,0,0)
  , editing = Nothing
  }

emptyCard : Card
emptyCard =
  Card 0 "text/markdown" ""

init : Maybe Data -> ( Model, Cmd Msg )
init savedData =
  case savedData of
    Nothing ->
      emptyModel ! [ ]
    Just data ->
      ( buildModel data ) ! [ ]



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
    [ text "test" ]



viewCard : Bool -> Bool -> Card -> Html Msg
viewCard isActive isEditing card =
  div
    [ classList [ ( "card", True )
                , ( "active", isActive )
                , ( "editing", isEditing )
                ]
    , id ( "card-" ++ toString card.id )
    ]
    [ div [ class "view" ] [ text card.content ]
    , input [ id ( "card-edit-" ++ toString card.id )
            , class "edit"
            , value card.content
            ]
            []
    ]


-- HELPERS


buildModel : Data -> Model
buildModel data =
  emptyModel


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
