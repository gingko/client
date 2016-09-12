port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra exposing (find)
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
  { structure : X
  , active : (Int, Int, Int)
  }
  
type alias Data =
  { content : List Content
  , aList : List A
  , active : (Int, Int, Int)
  , root : Id
  }

type alias Content =
  { id : Id
  , contentType : String
  , content : String
  }

type alias A =
  { id : Id
  , content : Id
  , children : List Id
  }

type X = 
  X
    { id : Id
    , content : Content
    , children : List X
    }

type alias Id = Int
type alias Group = List X
type alias Column = List (List X)


defaultModel : Model
defaultModel =
  { structure = defaultStructure
  , active = (0,0,0)
  }

defaultContent : Content
defaultContent =
  { id = 0
  , contentType = "text/markdown"
  , content = "defaultContent"
  }


defaultStructure : X
defaultStructure =
  X { id = 0
    , content = defaultContent
    , children= [ X {id = 1, content = defaultContent, children = []}
                , X {id = 2, content = defaultContent, children = []}
                ]
    }

init : Maybe Data -> ( Model, Cmd Msg )
init savedData =
  case savedData of
    Nothing ->
      defaultModel ! [ ]
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
  viewX defaultModel.structure


viewX : X -> Html Msg
viewX x =
  case x of
    X x' ->
      if x'.children == [] then
        div [ id "app" ] -- root Only
            [ viewColumn [[x]] ]
      else
        div [ id "app" ]
            [ viewColumn [[x]]
            , div [ class "column" ]
                  [ viewGroup x'.children ]
            ]


viewXContent : X -> Html Msg
viewXContent x =
  case x of
    X x' ->
      div [ class "card" ]
          [ text x'.content.content ]
    

viewGroup : Group -> Html Msg
viewGroup xs =
  div [ class "group" ]
      (List.map viewXContent xs)


viewColumn : Column -> Html Msg
viewColumn col =
  div [ class "column" ]
      (List.map viewGroup col)


-- STRUCTURING


getId : X -> Id
getId xin =
  case xin of
    X x ->
      x.id


xToA : X -> A
xToA x =
  case x of
    X x' ->
      { id = x'.id
      , content = x'.content.id
      , children = List.map getId x'.children
      }


aToX : Data -> A -> X
aToX data a =
  let
    fmFunction id = find (\a -> a.id == id) data.aList -- (Id -> Maybe A)
  in
    X { id = a.id
      , content = data.content  |> find (\c -> c.id == a.content)
                                |> Maybe.withDefault defaultContent
      , children = a.children -- List Id
                    |> List.filterMap fmFunction -- List A
                    |> List.map (aToX data) -- List X
      }


-- columnHasChildren : Column -> Bool
-- columnHasChildren col =
--   col |> List.concat
--       |> List.any (\x -> x.children /= [])

nextColumn : Column -> Column
nextColumn col = col


getColumns : List Column -> List Column
getColumns cols =
  cols
  -- if none of the last column X's have children, return cols
  -- otherwise, get the next column, append to cols, and return cols


buildModel : Data -> Model
buildModel data =
  { structure = data.aList -- List A
                  |> find (\a -> a.id == data.root) -- Maybe A
                  |> Maybe.withDefault (A 0 0 []) -- A
                  |> aToX data -- X
  , active = data.active
  }




--HELPERS


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
