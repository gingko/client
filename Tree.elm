module Tree exposing (..)

import String
import List.Extra as ListExtra
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)
import Json.Decode as Json
import Markdown

import Sha1
import Types exposing (..)




-- MODEL

type alias Tree =
  { uid : String
  , content : Content
  , prev : Maybe String
  , next : Maybe String
  , visible : Bool
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


default : Tree
default =
  { uid = "0"
  , content = Content (Sha1.sha1 ("" ++ newLine)) "" ""
  , children = Children [] 
  , next = Nothing
  , prev = Nothing 
  , visible = True 
  }




-- UPDATE

type Msg
  = NoOp
  | Activate String
  | UpdateCard String String
  | DeleteCard String
  | OpenCard String String
  | CancelCard
  | InsertBelow String
  | UpdateField String


update : Msg -> Tree -> Tree
update msg tree =
  case msg of
    NoOp -> tree

    UpdateCard uid str ->
      if tree.uid == uid then
         { tree | content = Content (Sha1.sha1 str) "" str }
      else
        case tree.children of
          Children [] ->
            tree
          Children trees ->
            { tree | children = Children (List.map (update (UpdateCard uid str)) trees) }

    DeleteCard uid ->
      if tree.uid == uid then
         { tree | visible = False }
      else
        case tree.children of
          Children [] ->
            tree
          Children trees ->
            { tree | children = Children (List.map (update (DeleteCard uid)) trees) }

    InsertBelow uid ->
      case tree.children of
        Children [] ->
          tree
        Children trees ->
          let
            blankTree = (Tree "1" (Content "" "" "") Nothing Nothing True (Children []))

            getNext : String -> Maybe String
            getNext tid =
              trees
                |> ListExtra.find (\t -> t.uid == tid)
                |> Maybe.withDefault blankTree
                |> .next


            newTree =
              { blankTree
                | prev = Just uid
                , next = getNext uid
                , uid = Sha1.sha1( uid ++ Maybe.withDefault "" (getNext uid))
              }

            allTrees = trees ++ [newTree]

            sortedChildrenIds =
              allTrees
                |> toDag
                |> linearizeDag

            sortedChildren =
              sortedChildrenIds
                |> List.filterMap (\cid -> ListExtra.find (\t -> t.uid == cid) allTrees) -- List Tree
                |> Children
          in
            if (List.member uid (List.map .uid trees)) then
              { tree
                | children = sortedChildren
              }
            else
              { tree | children = Children (List.map (update (InsertBelow uid)) trees) }

    _ ->
      tree




-- VIEW


viewColumn : ViewState -> Column -> Html Msg
viewColumn vstate col =
  div
    [ class "column" ]
    [ div
        [ class "buffer" ][]
    , div [](List.map (lazy (viewGroup vstate)) col)
    , div
        [ class "buffer" ][]
    ]


viewGroup : ViewState -> Group -> Html Msg
viewGroup vstate xs =
  div [ class "group" ]
      (List.map (lazy (viewCard vstate)) xs)


viewCard : ViewState -> Tree -> Html Msg
viewCard vstate tree =
    div [ id ("card-" ++ tree.uid)
        , classList [ ("card", True)
                    , ("active", vstate.active == tree.uid)
                    , ("editing", vstate.editing == Just tree.uid)
                    ]
        , onClick (Activate tree.uid)
        , onDoubleClick (OpenCard tree.uid tree.content.content)
        ]
        [ div [ class "view" ] [ Markdown.toHtml [] tree.content.content ]
        , button [ onClick (DeleteCard tree.uid) ][text "x"]
        , textarea
            [ id ( "card-edit-" ++ tree.uid )
            , class "edit"
            , value vstate.field
            , onBlur CancelCard
            , onInput UpdateField
            , onEnter (UpdateCard tree.uid vstate.field)
            ]
            []
        , button [ onClick (InsertBelow tree.uid) ][text "+"]
        ]



-- STRUCTURE FUNCTIONS


nodeToTree : Data -> String -> Node -> Tree
nodeToTree data uid a =
  let
    fmFunction id = ListExtra.find (\a -> a.id == id) data.nodes -- (String -> Maybe Node)
    imFunction = (\idx -> nodeToTree data (Sha1.sha1 ((toString idx) ++ uid)))
  in
    { uid = uid
    , content =
        data.contents
          |> ListExtra.find (\c -> c.id == (a.contentId))
          |> Maybe.withDefault defaultContent
    , children = a.childrenIds -- List String
                  |> List.filterMap fmFunction -- List Node
                  |> List.indexedMap imFunction -- List Tree
                  |> assignPrevNext -- List Tree
                  |> Children
    , next = Nothing
    , prev = Nothing
    , visible = True
    }




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
      cols ++ [[]]


buildStructure : Data -> Tree
buildStructure data =
  data.nodes -- List Node
    |> ListExtra.find (\a -> a.id == data.rootId) -- Maybe Node
    |> Maybe.withDefault (Node "0" "0" []) -- Node
    |> nodeToTree data "0" -- Tree


treeToNodes : List Node -> Tree -> List Node
treeToNodes nodes {uid, content, children} =
  case children of
    Children [] ->
      { id = Sha1.sha1(content.id ++ newLine )
      , contentId = content.id
      , childrenIds = []
      } :: nodes

    Children trees ->
      let
        descendants =
          trees
            |> filterByVisible
            |> List.map (treeToNodes nodes)

        childrenIds =
          trees
            |> filterByVisible
            |> List.map treeToNode -- TODO: recursion twice, likely costly unnecessary
            |> List.map .id
      in
        { id = Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))
        , contentId = content.id
        , childrenIds = childrenIds
        } :: nodes ++ (List.concat descendants)


treeToNode : Tree -> Node
treeToNode {uid, content, children} =
  case children of
    Children [] ->
      { id = Sha1.sha1(content.id ++ newLine)
      , contentId = content.id
      , childrenIds = []
      }

    Children trees ->
      let
        childrenIds =
          trees
            |> filterByVisible
            |> List.map treeToNode
            |> List.map .id
      in
        { id = Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))
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


getId : Tree -> String
getId {uid, content, children} =
  case children of
    Children [] ->
      Sha1.sha1(content.id ++ newLine )

    Children trees ->
      let
        childrenIds =
          trees
            |> filterByVisible
            |> List.map getId
      in
        Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))




-- POSET and DAG stuff


type alias DagEntry =
  { uid: String
  , prev: List String
  , next: List String
  }


toDag : List Tree -> List DagEntry
toDag posets =
  let
    getPrev : Tree -> List String
    getPrev pos =
      let
        prevId =
          case pos.prev of
            Nothing -> []
            Just puid -> [puid]
      in
        posets
          |> List.filter (\p -> p.next == Just pos.uid) -- List Tree
          |> List.map .uid -- List String
          |> List.append prevId
          |> ListExtra.unique -- List String

    getNext : Tree -> List String
    getNext pos =
      let
        nextId =
          case pos.next of
            Nothing -> []
            Just puid -> [puid]
      in
        posets
          |> List.filter (\p -> p.prev == Just pos.uid) -- List Tree
          |> List.map .uid -- List String
          |> List.append nextId
          |> ListExtra.unique -- List String

    f : Tree -> DagEntry
    f p = DagEntry p.uid (getPrev p) (getNext p)
  in
    posets -- List { uid: String, prev: Maybe String, next: Maybe String }
      |> List.map f


sortFunction : List DagEntry -> DagEntry -> DagEntry -> Order
sortFunction dag a b =
  let
    distA = (maxDist dag "end" a.uid)
    distB = (maxDist dag "end" b.uid)
  in
    if distA == distB then
      compare a.uid b.uid
    else
      compare distB distA


maxDist : List DagEntry -> String -> String -> Int
maxDist dag toId fromId =
  if toId == fromId then
    0
  else
    let
      maxNexts =
        dag -- List DagEntry
          |> ListExtra.find (\de -> de.uid == fromId) -- Maybe DagEntry
          |> Maybe.withDefault (DagEntry "end" ["start"] []) -- DagEntry
          |> .next
          |> List.map (maxDist dag toId)
          |> List.maximum
          |> Maybe.withDefault 0
    in
      1 + maxNexts


linearizeDag : List DagEntry -> List String
linearizeDag dag =
  dag -- List DagEntry
    |> List.sortWith (sortFunction dag) -- List DagEntry
    |> List.map .uid




--HELPERS

getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c
        |> filterByVisible


filterByVisible : List Tree -> List Tree
filterByVisible trees =
  trees
    |> List.filter (\t -> t.visible)


assignPrevNext : List Tree -> List Tree
assignPrevNext trees =
  let
    idList = trees |> List.map .uid

    imFunction : Int -> Tree -> Tree
    imFunction idx tree =
      { tree 
        | prev = ListExtra.getAt (idx - 1) idList
        , next = ListExtra.getAt (idx + 1) idList
      }

  in
    trees -- List Tree
      |> List.indexedMap imFunction


columnHasChildren : Column -> Bool
columnHasChildren col =
  col |> List.concat
      |> List.any (\x -> (getChildren x) /= [])


newLine : String
newLine =
  String.fromList ['\n']


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then
        msg
      else NoOp
  in
    on "keydown" (Json.map tagger keyCode)
