module Objects exposing (..)

import Dict exposing (Dict)
import Maybe exposing (andThen)
import Tuple exposing (first, second)

import Json.Encode as Enc
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required, optional)

import Types exposing (..)
import Sha1 exposing (sha1, timestamp)


-- MODEL

type alias Model =
  { treeObjects : Dict String TreeObject
  , commits : Dict String CommitObject
  , head : Head
  }


defaultModel : Model
defaultModel = Model Dict.empty Dict.empty (Head "master" "" "" [])


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


type alias Head =
  { id : String
  , current : String
  , previous : String
  , conflicts : List String
  }




-- UPDATE

type ObjMsg
  = Commit String Tree
  | SetHead String
  | In Json.Value
  | Merge
  | Merge3 String String String


update : ObjMsg -> Model -> Model
update msg model =
  let
    head = model.head
  in
  case msg of
    Commit author tree ->
      let
        parents =
          if model.head.current == "" then []
          else [model.head.current]

        (newHead, newModel) =
          commitWithParents author parents tree model
      in
      update (SetHead newHead) newModel

    SetHead newHead ->
      { model
        | head = { head | current = newHead, previous = head.previous }
      }

    In json ->
      case Json.decodeValue modelDecoder json of
        Ok modelIn ->
          let
            newModel =
              { model
                | treeObjects = Dict.union model.treeObjects modelIn.treeObjects
                , commits = Dict.union model.commits modelIn.commits
                , head = modelIn.head
              }
          in
          update Merge newModel

        Err err ->
          let _ = Debug.log "Objects.In json err:" err in
          model

    Merge ->
      case model.head.conflicts of
        [confHead] ->
          let
            _ = Debug.log "conflicts:" (model.head.current ++ ":" ++ confHead)
            merged =
              getCommonAncestor_ model.commits model.head.current confHead -- Maybe String
                |> Debug.log "conflicts:getCommonAncestor_"
                |> Maybe.map (\ca -> update (Merge3 ca model.head.current confHead) model)
                |> Maybe.withDefault model
          in
          merged

        _ ->
          model

    Merge3 o a b ->
      case merge o a b model of
        Ok m ->
          m

        _ ->
          model



commitWithParents : String -> List String -> Tree -> Model -> (String, Model)
commitWithParents author parents tree model =
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


previousCommit : Model -> Maybe String
previousCommit model =
  Dict.get model.head.current model.commits
    |> andThen (\co -> co.parents |> List.head)


nextCommit : Model -> Maybe String
nextCommit model =
  model.commits
    |> Dict.filter (\sha co -> List.member model.head.current co.parents)
    |> Dict.toList
    |> List.sortBy (\(sha, co) -> -1 * co.timestamp)
    |> List.map first
    |> List.head




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




-- PORTS & INTEROP

modelToValue : Model -> Enc.Value
modelToValue model =
  let
    commitToValue sha commit =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "commit" )
        , ( "tree", Enc.string commit.tree )
        , ( "parents", Enc.list (commit.parents |> List.map Enc.string) )
        , ( "author", Enc.string commit.author )
        , ( "timestamp", Enc.int commit.timestamp )
        ]

    treeObjectToValue sha treeObject =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "tree" )
        , ( "content", Enc.string treeObject.content )
        , ( "children", Enc.list
              (List.map (\(s, i) -> Enc.list [Enc.string s, Enc.string i]) treeObject.children) )
        ]

    commits =
      Dict.toList model.commits
        |> List.map (\(k, v) -> commitToValue k v)
        |> Enc.list

    treeObjects =
      Dict.toList model.treeObjects
        |> List.map (\(k, v) -> treeObjectToValue k v)
        |> Enc.list

    head =
      Enc.object
        [ ( "_id", Enc.string model.head.id )
        , ( "type", Enc.string "head" )
        , ( "current", Enc.string model.head.current )
        , ( "previous", Enc.string model.head.previous )
        ]

  in
  Enc.object
    [ ( "commits", commits )
    , ( "treeObjects", treeObjects )
    , ( "head", head )
    ]


modelDecoder : Json.Decoder Model
modelDecoder =
  Json.map3 Model
    ( Json.field "treeObjects" treeObjectsDecoder )
    ( Json.field "commits" commitsDecoder )
    ( Json.field "head" headDecoder )


commitsDecoder : Json.Decoder (Dict String CommitObject)
commitsDecoder =
  let
    commitObjectDecoder : Json.Decoder CommitObject
    commitObjectDecoder =
      Json.map4 CommitObject
        ( Json.field "tree" Json.string )
        ( Json.field "parents" (Json.list Json.string) )
        ( Json.field "author" Json.string )
        ( Json.field "timestamp" Json.int )
  in
  (Json.dict commitObjectDecoder)


treeObjectsDecoder : Json.Decoder (Dict String TreeObject)
treeObjectsDecoder =
  let
    tupleDecoder a b =
      Json.index 0 a
        |> Json.andThen
          (\aVal -> Json.index 1 b
              |> Json.andThen (\bVal -> Json.succeed (aVal, bVal))
          )

    treeObjectDecoder =
      Json.map2 TreeObject
        ( Json.field "content" Json.string )
        ( Json.field "children" (Json.list (tupleDecoder Json.string Json.string)) )
  in
  (Json.dict treeObjectDecoder)


headDecoder : Json.Decoder Head
headDecoder =
  decode Head
    |> required "_id" Json.string
    |> required "current" Json.string
    |> required "previous" Json.string
    |> optional "_conflicts" (Json.list Json.string) []




-- ==== Merging

getCommonAncestor_ : Dict String CommitObject -> String -> String -> Maybe String
getCommonAncestor_ commits shaA shaB =
  let
    _ = Debug.log "conflicts:shaA" shaA
    _ = Debug.log "conflicts:shaB" shaB

    getAncestors : Dict String CommitObject -> String -> List String
    getAncestors cm sh =
      let
        c_ = Dict.get sh cm
      in
      case c_ of
        Just c ->
          c.parents ++ (List.concatMap (getAncestors cm) c.parents)

        Nothing -> []

    aAncestors = getAncestors commits shaA
    bAncestors = getAncestors commits shaB
  in
  aAncestors
    |> List.filter (\a -> List.member a bAncestors)
    |> List.head


merge : String -> String -> String -> Model -> Result (List String) Model
merge o a b model =
  Ok model


mergeTree : (String, TreeObject) -> (String, TreeObject) -> (String, TreeObject) -> Result (List String) (String, TreeObject)
mergeTree oTree aTree bTree =
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

