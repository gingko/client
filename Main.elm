port module Main exposing (..)


import Dom
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import String
import List.Extra as ListExtra
import Task


main : Program (Maybe Model)
main =
  App.programWithFlags
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }




-- MODEL


type alias Model =
  { contents : List Content
  , nodes : List Node
  , viewState : ViewState
  , rootId : Id
  }

type alias ViewState =
  { active : Uid
  }

type alias Content =
  { id : Id
  , contentType : String
  , content : String
  }

type alias Node =
  { id : Id
  , contentId : Id
  , childrenIds : List Id
  }

type alias Tree = 
  { uid : Uid
  , content : Content
  , children : Children
  }

type Children = Children (List Tree)
type alias Id = String
type alias Uid = Int
type alias Group = List Tree
type alias Column = List (List Tree)


defaultContent : Content
defaultContent =
  { id = "0"
  , contentType = "text/markdown"
  , content = "defaultContent"
  }


defaultModel : Model
defaultModel =
  { contents = [defaultContent, { defaultContent | id = "1", content = "2" }]
  , nodes = [Node "0" "0" ["1"], Node "1" "1" []]
  , viewState = ViewState 0
  , rootId = "0"
  }


init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  case savedModel of
    Nothing ->
      defaultModel ! [ ]
    Just data ->
      data ! [ ]




-- UPDATE


type Msg
    = NoOp
    | Activate Uid
    | ClearContent Uid


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      model ! []

    Activate uid ->
      { model | viewState = ViewState uid }
        ! []

    ClearContent uid ->
      let
        newStructure = Debug.log "newStructure" ( updateTree (ClearContent uid) (buildStructure model) )
        newModel = Debug.log "newModel" (buildModel newStructure)
      in
        { model
          | nodes = model.nodes ++ newModel.nodes |> ListExtra.uniqueBy (\n -> n.id)
          , contents = model.contents ++ newModel.contents |> ListExtra.uniqueBy (\c -> c.id)
          , rootId = newModel.rootId
        }
          ! []



-- VIEW


view : Model -> Html Msg
view model =
  viewTree model.viewState (buildStructure model)


viewTree : ViewState -> Tree -> Html Msg
viewTree vs x =
  let
    columns = getColumns([[[x]]])
  in
    div [ id "app" ]
        (List.map (viewColumn vs) columns)


viewCard : ViewState -> Tree -> Html Msg
viewCard vs x =
    div [ id ("card-" ++ (toString x.uid))
        , classList [("card", True), ("active", vs.active == x.uid)]
        , onClick (Activate x.uid)
        , onDoubleClick (ClearContent x.uid)
        ]
        [ text x.content.content ]
    

viewGroup : ViewState -> Group -> Html Msg
viewGroup vs xs =
  div [ class "group" ]
      (List.map (viewCard vs) xs)


viewColumn : ViewState -> Column -> Html Msg
viewColumn vs col =
  div [ class "column" ]
      (List.map (viewGroup vs) col)


-- STRUCTURING


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c


nodeToTree : Model -> Int -> Node -> Tree
nodeToTree model uid a =
  let
    fmFunction id = ListExtra.find (\a -> a.id == id) model.nodes -- (Id -> Maybe Node)
    imFunction = (\idx -> nodeToTree model (idx + uid + 1))
  in
    { uid = uid
    , content = model.contents |> ListExtra.find (\c -> c.id == a.contentId)
                              |> Maybe.withDefault defaultContent
    , children = a.childrenIds -- List Id
                  |> List.filterMap fmFunction -- List Node
                  |> List.indexedMap imFunction -- List Tree
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
    col = case (ListExtra.last cols) of
      Nothing -> [[]]
      Just c -> c
    hasChildren = columnHasChildren col
  in
    if hasChildren then
      getColumns(cols ++ [nextColumn(col)])
    else
      cols


buildStructure : Model -> Tree
buildStructure model =
  model.nodes -- List Node
    |> ListExtra.find (\a -> a.id == model.rootId) -- Maybe Node
    |> Maybe.withDefault (Node "0" "0" []) -- Node
    |> nodeToTree model 0 -- Tree


treeToNodes : List Node -> Tree -> List Node
treeToNodes nodes {uid, content, children} =
  case children of
    Children [] ->
      { id = content.id
      , contentId = content.id
      , childrenIds = []
      } :: nodes

    Children trees ->
      let
        descendants = 
          trees 
            |> List.map (treeToNodes nodes)

        childrenIds =
          trees
            |> List.map treeToNode -- TODO: recursion twice, likely costly unnecessary
            |> List.map .id
      in
        { id = content.id ++ (String.concat childrenIds)
        , contentId = content.id
        , childrenIds = childrenIds
        } :: nodes


treeToNode : Tree -> Node
treeToNode {uid, content, children} =
  case children of
    Children [] ->
      { id = content.id
      , contentId = content.id
      , childrenIds = []
      }

    Children trees ->
      let
        childrenIds = 
          trees 
            |> List.map treeToNode -- List Node
            |> List.map .id
      in
        { id = content.id ++ (String.concat childrenIds)
        , contentId = content.id
        , childrenIds = childrenIds
        }


getContents : Tree -> List Content
getContents {uid, content, children} =
  case children of
    Children [] ->
      [content]

    Children trees ->
      [content] ++ (List.concatMap getContents trees)


updateTree : Msg -> Tree -> Tree
updateTree msg tree =
  case msg of
    ClearContent uid ->
      if tree.uid == uid then
        { tree | content = Content "text/markdown" "text/markdown" "" }
      else
        case tree.children of
          Children [] ->
            tree
          Children trees ->
            { tree | children = Children (List.map (updateTree (ClearContent uid)) trees) }

    _ ->
      tree


buildModel : Tree -> Model
buildModel tree =
  let
    nodes = (treeToNodes []) tree
  in
    { contents = getContents tree
    , nodes = nodes
    , rootId =
        nodes 
          |> List.head
          |> Maybe.withDefault (Node "0" "" [])
          |> .id
    , viewState = { active = 0}
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
