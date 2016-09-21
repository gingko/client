module TreeUtils exposing (..)

import String
import List.Extra as ListExtra
import Types exposing (..)
import Sha1




-- TREE AND NODE TRANSFORMATIONS

nodeToTree : Data -> String -> Node -> Tree
nodeToTree data uid a =
  let
    fmFunction id = ListExtra.find (\a -> a.id == id) data.nodes -- (String -> Maybe Node)
    imFunction = (\idx -> nodeToTree data (nextUid uid idx))
  in
    { uid = uid
    , content =
        data.contents
          |> ListExtra.find (\c -> c.id == (a.contentId))
          |> Maybe.withDefault defaultContent
    , parentId = Nothing
    , children = a.childrenIds -- List String
                  |> List.filterMap fmFunction -- List Node
                  |> List.indexedMap imFunction -- List Tree
                  |> assignPrevNext -- List Tree
                  |> List.map (\t -> { t | parentId = Just uid})
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
      { id = nodeId content.id []
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
        { id = nodeId content.id childrenIds
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




-- ACCESSORS

getContents : Tree -> List Content
getContents {uid, content, children} =
  case children of
    Children [] ->
      [content]

    Children trees ->
      [content] ++ (List.concatMap getContents trees)


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c
        |> filterByVisible


getNextVisible : List Tree -> String -> Maybe String
getNextVisible trees uid =
  let
    ids = trees |> filterByVisible |> List.map .uid
    idx = ids |> ListExtra.elemIndex uid
        
  in
    case idx of
      Nothing ->
        Nothing

      Just i ->
        ListExtra.getAt (i+1) ids




-- UID FUNCTIONS

withContentId : Content -> Content
withContentId {id, contentType, content} =
  { id = Sha1.sha1 (contentType ++ newLine ++ content)
  , contentType = contentType
  , content = content
  }


nodeId : String -> List String -> String
nodeId contentId children =
  Sha1.sha1 (contentId ++ newLine ++ (String.concat children))


nextUid : String -> Int -> String
nextUid uid idx =
  Sha1.sha1 (uid ++ (toString idx))


-- opUid : Tree -> Msg -> String
-- opUid tree msg =
  -- case msg of
    -- InsertBelow uid ->




newUid : Maybe String -> Maybe String -> Maybe String -> String
newUid parentId prevId nextId =
  let
    pid = Maybe.withDefault "" parentId
    vid = Maybe.withDefault "" prevId
    nid = Maybe.withDefault "" nextId
  in
    String.concat [pid ++ newLine ++ vid ++ newLine ++ nid]
      |> Sha1.sha1


treeUid : Tree -> String
treeUid {uid, content, children} =
  case children of
    Children [] ->
      Sha1.sha1(content.id ++ newLine )

    Children trees ->
      let
        childrenIds =
          trees
            |> List.filter (\t -> t.visible)
            |> List.map treeUid
      in
        Sha1.sha1(content.id ++ newLine ++ (String.concat childrenIds))



-- HELPERS

filterByVisible : List Tree -> List Tree
filterByVisible trees =
  trees
    |> List.filter (\t -> t.visible)



columnHasChildren : Column -> Bool
columnHasChildren col =
  col |> List.concat
      |> List.any (\x -> (getChildren x) /= [])


newLine : String
newLine =
  String.fromList ['\n']

