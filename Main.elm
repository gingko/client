port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import List.Extra exposing (find, last)
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

type alias X = 
  { id : Id
  , content : Content
  , children : Children
  }

type Children = Children (List X)
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
  { id = 0
  , content = defaultContent
  , children = Children [ { id = 1
                          , content = defaultContent
                          , children = Children [ {id = 1, content = defaultContent, children = Children []}
                                                , {id = 2, content = defaultContent, children = Children []}
                                                ]
                          }
                        , {id = 2, content = defaultContent, children = Children []}
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
  let
    columns = getColumns([[[x]]])
  in
    div [ id "app" ]
        (List.map viewColumn columns)


viewXContent : X -> Html Msg
viewXContent x =
    div [ class "card" ]
        [ text x.content.content ]
    

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
getId x =
  x.id


getChildren : X -> List X
getChildren x =
  case x.children of
    Children c ->
      c


xToA : X -> A
xToA x =
  { id = x.id
  , content = x.content.id
  , children = List.map getId (getChildren x)
  }


aToX : Data -> A -> X
aToX data a =
  let
    fmFunction id = find (\a -> a.id == id) data.aList -- (Id -> Maybe A)
  in
    { id = a.id
    , content = data.content  |> find (\c -> c.id == a.content)
                              |> Maybe.withDefault defaultContent
    , children = a.children -- List Id
                  |> List.filterMap fmFunction -- List A
                  |> List.map (aToX data) -- List X
                  |> Children
    }


columnHasChildren : Column -> Bool
columnHasChildren col =
  col |> List.concat
      |> List.any (\x -> (getChildren x) /= [])


nextColumn : Column -> Column
nextColumn col =
  (List.map getChildren (List.concat col))


getColumns : List Column -> List Column
getColumns cols =
  let
    col = case (last cols) of
      Nothing -> [[]]
      Just c -> c
    hasChildren = columnHasChildren col
  in
    if hasChildren then
      getColumns(cols ++ [nextColumn(col)])
    else
      cols


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
