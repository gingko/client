module Objects exposing (..)

import Dict exposing (Dict)
import Maybe exposing (andThen)
import Tuple exposing (first, second)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
  , children : List (String, String) -- List (sha, tree id)
  }


type alias CommitObject =
  { tree : String
  , parents : List String
  , author : String
  , timestamp : Int
  }




-- Commit Saving & Loading

commit : String -> List String -> Tree -> Model -> (String, Model)
commit author parents tree model =
  let
    (newRootId, newRootTree) =
      treeToObject tree

    newCommit = CommitObject
      newRootId
      parents
      author
      (timestamp ())

    newCommitSha = newCommit |> commitSha

    newTreeObjects =
      generateObjects tree
  in
  ( newCommitSha
  , { model
      | commits = Dict.insert newCommitSha newCommit model.commits
      , treeObjects = Dict.union model.treeObjects newTreeObjects
    }
  )


loadCommit : String -> Model -> Maybe Tree
loadCommit commitSha model =
  Dict.get commitSha model.commits
    |> andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")


parentCommit : String -> Model -> Maybe String
parentCommit commitSha model =
  Dict.get commitSha model.commits
    |> andThen (\co -> co.parents |> List.head)


childCommit : String -> Model -> Maybe String
childCommit commitSha model =
  model.commits
    |> Dict.filter (\sha co -> List.member commitSha co.parents)
    |> Dict.toList
    |> List.map first
    |> List.head




-- VIEW

view : String -> Model -> Html Msg
view head model =
  div [id "history"
      ]
      [ ul
          []
          (model.commits
            |> Dict.toList
            |> List.sortBy (\(sha, commit) -> commit.timestamp)
            |> List.reverse
            |> List.map (viewCommit head)
          )
      ]

viewCommit : String -> (String, CommitObject) -> Html Msg
viewCommit head (sha, commit) =
  li
    [ classList [("active", sha == head)]
    , onClick (LoadCommit sha)
    ]
    [text (sha ++ ":" ++ (commit.timestamp |> toString))]





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


treeObjectsToTree : Dict String TreeObject -> String -> String -> Maybe Tree
treeObjectsToTree treeObjects treeSha id =
  let
    treeObject_ =
      Dict.get treeSha treeObjects
  in
  case treeObject_ of
    Just {content, children} ->
      let
        fMap (sh, i) =
          treeObjectsToTree treeObjects sh i

        subtrees =
          children
            |> List.filterMap fMap -- List Tree
            |> Children
      in
      Just (Tree id content subtrees)

    Nothing -> Nothing


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

