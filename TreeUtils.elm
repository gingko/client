module TreeUtils exposing (..)

import String
import List.Extra as ListExtra
import Types exposing (..)
import Sha1




-- TREE AND NODE TRANSFORMATIONS

nodeToTree : Objects -> String -> Node -> Tree
nodeToTree objects uid a =
  let
    fmFunction id = ListExtra.find (\a -> a.id == id) objects.nodes -- (String -> Maybe Node)
    imFunction = (\idx -> nodeToTree objects (nextUid uid idx))
  in
    { uid = uid
    , content =
        objects.contents
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


buildStructure : String -> Objects -> Tree
buildStructure nodeId objects =
  objects.nodes -- List Node
    |> ListExtra.find (\a -> a.id == nodeId) -- Maybe Node
    |> Maybe.withDefault (Node "0" "0" []) -- Node
    |> nodeToTree objects "0" -- Tree


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

getTree : Tree -> String -> Maybe Tree
getTree tree uid =
  tree
    |> getDescendants
    |> ListExtra.find (\t -> t.uid == uid)


getContent : Tree -> String -> Maybe Content
getContent {uid, content, children} id =
  if uid == id then
    Just content
  else
    case children of
      Children [] ->
        Nothing

      Children trees ->
        trees
          |> List.map (flip getContent id)
          |> Maybe.oneOf


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

getDescendants : Tree -> List Tree
getDescendants t =
  let
    children = getChildren t
  in
  if List.isEmpty children then
    []
  else
    children ++ (List.concatMap getDescendants children)


getNext : Tree -> String -> Maybe String
getNext tree uid =
  case tree.children of
    Children children ->
      if (List.member uid (List.map .uid children)) then -- if uid is in children
        let
          ids = children |> filterByVisible |> List.map .uid
          idx = ids |> ListExtra.elemIndex uid
              
        in
          case idx of
            Nothing ->
              Nothing

            Just i ->
              ListExtra.getAt (i+1) ids
      else
        children -- List Tree
          |> List.map ((flip getNext) uid)
          |> Maybe.oneOf


getParent : Tree -> String -> Maybe String
getParent tree uid =
  case tree.children of
    Children [] ->
      Nothing
    Children children ->
      -- if the children contains uid then this is the parent
      if (List.member uid (List.map .uid children)) then
        Just tree.uid
      else
        children -- List Tree
          |> List.map ((flip getParent) uid)
          |> Maybe.oneOf


getLastChild : Tree -> String -> Maybe String
getLastChild tree uid =
  case tree.children of
    Children children ->
      if tree.uid == uid then
        children
          |> filterByVisible
          |> List.map .uid
          |> ListExtra.last
      else
        children
          |> List.map ((flip getLastChild) uid)
          |> Maybe.oneOf





-- UID FUNCTIONS

withContentId : Content -> Content
withContentId {id, contentType, content} =
  { id = Sha1.sha1 (contentType ++ newLine ++ content)
  , contentType = contentType
  , content = content
  }

withCommitId : Commit -> Commit
withCommitId c =
  { c
    | id =
        c.rootNode ++ newLine ++ 
        (String.join newLine c.parents) ++
        (String.join newLine c.authors) ++
        c.committer ++ newLine ++
        (toString c.timestamp) ++ newLine ++
        c.message
          |> Sha1.sha1
  }

nodeId : String -> List String -> String
nodeId contentId children =
  Sha1.sha1 (contentId ++ newLine ++ (String.concat children))


nextUid : String -> Int -> String
nextUid uid idx =
  Sha1.sha1 (uid ++ (toString idx))


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

