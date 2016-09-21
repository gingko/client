module Tree exposing (..)

import String
import List.Extra as ListExtra
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
  | UpdateCard String String
  | DeleteCard String
  | InsertBelow String


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


-- VIEW



-- HELPERS


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c
        |> filterTrees


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
            |> filterTrees
            |> List.map (treeToNodes nodes)

        childrenIds =
          trees
            |> filterTrees
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
            |> filterTrees
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
            |> filterTrees
            |> List.map getId
      in
        Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))


filterTrees : List Tree -> List Tree
filterTrees trees =
  trees
    |> List.filter (\t -> t.visible)



-- POSET and DAG stuff


type alias PosetEntry =
  { uid: String
  , prev: Maybe String
  , next: Maybe String
  }


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


testDag : List DagEntry
testDag =
  [ DagEntry "start" [] ["1"]
  , DagEntry "1" ["start"] ["2","end"]
  , DagEntry "2" ["1"] ["3", "end"]
  , DagEntry "3" ["2"] ["4", "end"]
  , DagEntry "4" ["3"] ["end"]
  , DagEntry "end" ["1","2","3","4"] []
  ]


testPoset : List PosetEntry
testPoset =
  [ PosetEntry "start" Nothing (Just "end")
  , PosetEntry "end" (Just "start") Nothing
  , PosetEntry "a" (Just "start") (Just "end") 
  , PosetEntry "b" (Just "a") (Just "end") 
  , PosetEntry "c" (Just "b") (Just "end") 
  , PosetEntry "y" (Just "b") (Just "c") 
  , PosetEntry "z" (Just "b") (Just "c") 
  , PosetEntry "x" (Just "a") (Just "b") 
  , PosetEntry "f" (Just "x") (Just "y") 
  ]


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


newLine : String
newLine =
  String.fromList ['\n']


