module TreeUtils exposing (..)

import String
import Dict exposing (..)
import Tuple exposing (first, second)
import List.Extra as ListExtra
import Types exposing (..)
import Random exposing (initialSeed, int, minInt, maxInt)


-- TRANSFORMATIONS

getColumns : List Column -> List Column
getColumns cols =
  let
    col =
      case (ListExtra.last cols) of
        Nothing -> [[]]
        Just c -> c

    hasChildren =
      col
        |> List.concat
        |> List.any (\x -> (getChildren x) /= [])

    nextColumn col =
      List.map getChildren (List.concat col)
  in
  if hasChildren then
    getColumns(cols ++ [nextColumn(col)])
  else
    cols





-- ACCESSORS

getTree : String -> Tree -> Maybe Tree
getTree id tree =
  if tree.id == id then
    Just tree
  else
    getChildren tree
      |> List.map (getTree id)
      |> List.filter (\m -> m /= Nothing)
      |> List.head
      |> Maybe.withDefault Nothing

getParent : String -> Tree -> Maybe Tree
getParent id tree =
  case tree.children of
    Children [] ->
      Nothing
    Children children ->
      if (List.member id (List.map .id children)) then
        Just tree
      else
        children
          |> List.map (getParent id)
          |> List.filter (\m -> m /= Nothing)
          |> List.head
          |> Maybe.withDefault Nothing


getChildren : Tree -> List Tree
getChildren x =
  case x.children of
    Children c ->
      c


getSiblings : String -> Tree -> List Tree
getSiblings id tree =
  if (getChildren tree |> List.map .id |> List.member id) then
    getChildren tree
  else
    List.concatMap (getSiblings id) (getChildren tree)


getColumn : Int -> Tree -> Maybe (List (List Tree))
getColumn n tree =
  let
    cols =
      getColumns [[[tree]]]
  in
  ListExtra.getAt n cols


getTreeWithPosition : String -> Tree -> Maybe (Tree, String, Int)
getTreeWithPosition id tree =
  Maybe.map3
    (\t p i -> (t, p, i))
    ( getTree id tree )
    ( getParent id tree |> Maybe.map .id )
    ( getIndex id tree )


getPrevNext : Int -> String -> Tree -> Maybe Tree
getPrevNext shift id tree =
  let
    siblings = getSiblings id tree
    idx =
      siblings
        |> List.map .id
        |> ListExtra.elemIndex id
  in
  case idx of
    Nothing -> Nothing

    Just i ->
      siblings
        |> ListExtra.getAt (i + shift)


getPrev : String -> Tree -> Maybe Tree
getPrev id tree =
  getPrevNext (-1) id tree


getNext : String -> Tree -> Maybe Tree
getNext id tree =
  getPrevNext 1 id tree


getPrevNextInColumn : Int -> String -> Tree -> Maybe Tree
getPrevNextInColumn shift id tree =
  let
    n = getDepth 0 tree id
    column_ = getColumn n tree
  in
  case column_ of
    Nothing -> Nothing

    Just col ->
      let
        idx =
          col
            |> List.concat
            |> List.map .id
            |> ListExtra.elemIndex id
      in
      case idx of
        Nothing -> Nothing

        Just i ->
          col
            |> List.concat
            |> ListExtra.getAt (i + shift)


getPrevInColumn : String -> Tree -> Maybe Tree
getPrevInColumn id tree =
  getPrevNextInColumn (-1) id tree


getNextInColumn : String -> Tree -> Maybe Tree
getNextInColumn id tree =
  getPrevNextInColumn 1 id tree


getContent : String -> Tree -> String
getContent id tree =
  case getTree id tree of
    Nothing ->
      ""
    Just t ->
      t.content


getIndex : String -> Tree -> Maybe Int
getIndex id tree =
  getSiblings id tree
    |> List.map .id
    |> ListExtra.elemIndex id


getDescendants : Tree -> List Tree
getDescendants t =
  let
    children = getChildren t
  in
  if List.isEmpty children then
    []
  else
    children ++ (List.concatMap getDescendants children)


getAncestors : Tree -> Tree -> List Tree -> List Tree
getAncestors all target accum =
  let
    current =
      case (List.head accum) of
        Nothing -> target
        Just t -> t
  in
  case (getParent current.id all) of
    Nothing -> accum
    Just p ->
      (getAncestors all target (p :: accum))


getDepth : Int -> Tree -> String -> Int
getDepth prev tree id =
  case tree.children of
    Children children ->
      if (tree.id == id) then
        prev
      else
        children
          |> List.map ((flip (getDepth (prev+1))) id)
          |> List.maximum
          |> Maybe.withDefault 0




-- DATABASE TRANSFORMATIONS


generateId : String -> Int -> String
generateId timeString time =
  [ "node"
  , timeString
  , Random.step (int 0 maxInt) (initialSeed time)
      |> first
      |> toString
  ]
    |> String.join "-"




-- SPECIAL PROPERTIES

centerlineIds : List (List String) -> List String -> List String -> List (List String)
centerlineIds flatCols allIds activePast =
  let
    lastActiveOrAll aP ids =
      let
        lastActiveIdx_ =
          aP
            |> ListExtra.findIndex (\a -> List.member a ids)
      in
      case lastActiveIdx_ of
        Nothing -> ids
        Just idx ->
          aP
            |> ListExtra.getAt idx -- Maybe String
            |> Maybe.withDefault "1"
            |> ListExtra.singleton
  in
  flatCols
    |> List.drop 1
    |> List.map (\c -> List.filter (\id -> List.member id allIds) c)
    |> ListExtra.filterNot List.isEmpty
    |> List.map (lastActiveOrAll activePast)




-- HELPERS

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?

newLine : String
newLine =
  String.fromList ['\n']

withIdTree : String -> Tree
withIdTree id =
  Tree id "" (Children [])

dictUpdate : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdate id upd dict =
  Dict.update
    id
    (\n_ ->
      case n_ of
        Just n -> Just (upd n)
        Nothing -> Nothing
    )
    dict
