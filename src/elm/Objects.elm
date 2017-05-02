module Objects exposing (..)

import Dict exposing (Dict)

import Types exposing (..)
import Sha1 exposing (sha1, timestamp)


-- MODEL

type alias Model =
  { treeObjects : Dict String TreeObject
  , commits : Dict String CommitObject
  }


defaultModel : Model
defaultModel = Model Dict.empty Dict.empty


type alias TreeObject =
  { content : String
  , children : List (String, String)
  }


type alias CommitObject =
  { tree : String
  , author : String
  , timestamp : Int
  , parents : List String
  }




-- UPDATE

commit : String -> List String -> Tree -> Model -> Model
commit author parents tree model =
  let
    (newRootId, newRootTree) =
      treeToObject tree

    newCommit = CommitObject
      newRootId
      author
      (timestamp ())
      parents

    newTreeObjects =
      generateObjects tree
  in
  { model
    | commits = Dict.insert (newCommit |> commitSha) newCommit model.commits
    , treeObjects = Dict.union model.treeObjects newTreeObjects
  }




-- ==== Generating Objects

generateObjects : Tree -> Dict String TreeObject
generateObjects tree =
  case tree.children of
    Children treeList ->
      let
        rootDict =
          treeToObject tree
            |> List.singleton
            |> Dict.fromList
      in
      treeList
        |> List.map generateObjects
        |> List.foldr Dict.union rootDict


treeToObject : Tree -> (String, TreeObject)
treeToObject tree =
  case treeToObjectId tree of
    (sha, id, treeObj) ->
      (sha, treeObj)


treeToObjectId : Tree -> (String, String, TreeObject)
treeToObjectId {id, content, children} =
  case children of
    Children [] ->
      (content ++ "\n" |> sha1, id, TreeObject content [])

    Children treeList ->
      let
        childrenIds =
          treeList
            |> List.map treeToObjectId
            |> List.map (\(id, u, obj) -> (id, u))
      in
      ( content ++ "\n" ++
        ( childrenIds
          |> List.map (\(i, u) -> i ++ " " ++ u)
          |> String.join "\n"
        )
          |> sha1
      , id
      , TreeObject content childrenIds
      )


-- SHA IDs

commitSha : CommitObject -> String
commitSha commit =
  ( commit.tree ++ "\n" ) ++
  ( commit.parents |> String.join "\n" ) ++
  ( commit.author ++ " " ++ ( commit.timestamp |> toString ) )
    |> sha1




-- ==== Merging

merge : (String, TreeObject) -> (String, TreeObject) -> (String, TreeObject) -> Result (List String) (String, TreeObject)
merge oTree aTree bTree =
  Ok ("fakesha", TreeObject "" [])


mergeStrings : String -> String -> String -> Result String String
mergeStrings o a b =
  let
    mergeFn x y z =
      "string conflict:" ++
      x ++"\n" ++
      y ++"\n" ++
      z
        |> Err
  in
  mergeGeneric mergeFn o a b


mergeTreeList : List (String, String) -> List (String, String) -> List (String, String) -> Result String (List (String, String))
mergeTreeList oList aList bList =
  let
    mergeFn x y z =
      Ok y
  in
  mergeGeneric mergeFn oList aList bList


mergeGeneric : (a -> a -> a -> Result String a) -> a -> a -> a -> Result String a
mergeGeneric mergeFn o a b =
  if a == b then
    Ok a
  else if o == b && o /= a then
    Ok a
  else if o == a && o /= b then
    Ok b
  else
    mergeFn o a b

