module Objects exposing (..)

import Dict exposing (Dict)
import Maybe exposing (andThen)
import Tuple exposing (first, second)
import List.Extra as ListExtra

import Json.Encode as Enc
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required, optional)

import Types exposing (..)
import TreeUtils exposing (getChildren)
import Sha1 exposing (sha1, timestamp)


-- MODEL

type alias Model =
  { commits : Dict String CommitObject
  , treeObjects : Dict String TreeObject
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
  | CommitMerge (List String) (List String) Tree
  | SetHead String
  | Change Json.Value
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

    CommitMerge authors parents tree ->
      let
        (newHead, newModel) =
          commitWithParents (authors |> String.join " ") parents tree model
      in
      update (SetHead newHead) newModel

    SetHead newHead ->
      { model
        | head = { head | current = newHead, previous = head.current }
      }

    Change json ->
      case Json.decodeValue (changeDecoder model) json of
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
          let _ = Debug.log "Objects.Change json err:" err in
          model



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
      let
        mergeFold commitSha prevModel =
          getCommonAncestor_ prevModel.commits commitSha prevModel.head.current
            |> Maybe.map (\ca -> update (Merge3 ca commitSha prevModel.head.current) prevModel)
            |> Maybe.withDefault prevModel

        merged =
          List.foldr mergeFold model model.head.conflicts

        mergedHead = merged.head
      in
      { merged | head = { mergedHead | conflicts = [] }}

    Merge3 oSha aSha bSha ->
      let
        aCommit_ = Dict.get aSha model.commits
        bCommit_ = Dict.get bSha model.commits
      in
      case (merge oSha aSha bSha model, aCommit_, bCommit_) of
        (Ok tree, Just aCommit, Just bCommit) ->
          update (CommitMerge [aCommit.author, bCommit.author] [aSha, bSha] tree) model

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
    treeObjectToValue sha treeObject =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "tree" )
        , ( "content", Enc.string treeObject.content )
        , ( "children", Enc.list
              (List.map (\(s, i) -> Enc.list [Enc.string s, Enc.string i]) treeObject.children) )
        ]

    commits =
      commitsToValue model.commits

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


commitsToValue : Dict String CommitObject -> Enc.Value
commitsToValue commits =
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

  in
  Dict.toList commits
    |> List.map (\(k, v) -> commitToValue k v)
    |> Enc.list


modelDecoder : Json.Decoder Model
modelDecoder =
  Json.map3 Model
    ( Json.field "commits" commitsDecoder )
    ( Json.field "treeObjects" treeObjectsDecoder )
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


changeDecoder : Model -> Json.Decoder Model
changeDecoder model =
  Json.oneOf
    [ Json.map3 Model
        ( Json.succeed model.commits )
        ( Json.succeed model.treeObjects )
        headDecoder
    , Json.map3 Model
        ( Json.succeed model.commits )
        treeObjectsDecoder
        ( Json.succeed model.head )
    , Json.map3 Model
        commitsDecoder
        ( Json.succeed model.treeObjects )
        ( Json.succeed model.head )
    ]




-- ==== Merging

getCommonAncestor_ : Dict String CommitObject -> String -> String -> Maybe String
getCommonAncestor_ commits shaA shaB =
  let
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


merge : String -> String -> String -> Model -> Result (List String) Tree
merge oSha aSha bSha model =
  let
    getTree_ sha =
      Dict.get sha model.commits
        |> Maybe.andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")

    oTree_ = getTree_ oSha
    aTree_ = getTree_ aSha
    bTree_ = getTree_ bSha
  in
  case (oTree_, aTree_, bTree_) of
    (Just oTree, Just aTree, Just bTree) ->
      let
        mTree = mergeTrees oTree aTree bTree
          |> Result.withDefault oTree
      in
      Ok mTree

    _ ->
      Err ["Couldn't find all trees for 3 way merge"]


mergeTrees : Tree -> Tree -> Tree -> Result (List String) Tree
mergeTrees oTree aTree bTree =
  let
    mContent = mergeStrings oTree.content aTree.content bTree.content
      |> Result.withDefault "mergeString conflict"

    mChildren = mergeChildren (getChildren oTree) (getChildren aTree) (getChildren bTree)
      |> Result.withDefault ((getChildren oTree)++(getChildren aTree)++(getChildren bTree))
      |> Children
  in
  Ok (Tree oTree.id mContent mChildren)

type MergeColumn = O | A | B
type alias MergeDict = Dict String (Maybe Tree, Maybe Tree, Maybe Tree)


mergeChildren : List Tree -> List Tree -> List Tree -> Result (List String) (List Tree)
mergeChildren oList aList bList =
  let
    allTrees = oList ++ aList ++ bList

    oVals = getTreeDict O oList
    aVals = getTreeDict A aList
    bVals = getTreeDict B bList

    mbHelper l r =
      case (l, r) of
        (Just a, Just b) -> Just a
        (Just a, Nothing) -> Just a
        (Nothing, Just b) -> Just b
        (Nothing, Nothing) -> Nothing

    bothStep id (lo, la, lb) (ro, ra, rb) dict =
      Dict.insert id (mbHelper lo ro, mbHelper la ra, mbHelper lb rb) dict

    allVals =
      Dict.merge Dict.insert bothStep Dict.insert oVals aVals Dict.empty
      |> (\di -> Dict.merge Dict.insert bothStep Dict.insert di bVals Dict.empty)

    mergedTrees =
      allVals
        |> Dict.toList -- List (String, (MbT, MbT, MbT))
        |> List.filterMap (\(id, (o_, a_, b_)) ->
            case (o_, a_, b_) of
              (Just ot, Just at, Just bt) ->
                mergeTrees ot at bt
                  |> Result.toMaybe

              (Just ot, Just at, Nothing) ->
                if ot == at then
                  Nothing
                else
                  Just at -- TODO: delete/modify conflict

              (Just ot, Nothing, Just bt) ->
                if ot == bt then
                  Nothing
                else
                  Just bt -- TODO: delete/modify conflict

              (Nothing, Nothing, Just bt) ->
                Just bt

              (Nothing, Just at, Nothing) ->
                Just at

              (Nothing, Just at, Just bt) ->
                if at == bt then
                  Just at
                else
                  Just at -- TODO: modify/modify conflict?

              _ ->
                Debug.crash "impossible state?"

          )

  in
  Ok mergedTrees


getTreeDict : MergeColumn -> List Tree -> MergeDict
getTreeDict col trees =
  case col of
    O -> trees |> List.map (\t -> (t.id, (Just t, Nothing, Nothing)) ) |> Dict.fromList
    A -> trees |> List.map (\t -> (t.id, (Nothing, Just t, Nothing)) ) |> Dict.fromList
    B -> trees |> List.map (\t -> (t.id, (Nothing, Nothing, Just t)) ) |> Dict.fromList



mergeStrings : String -> String -> String -> Result String String
mergeStrings o a b =
  let
    mergeFn x y z =
      "theirs:\n```\n" ++
      y ++ "\n```\nyours:\n```\n" ++
      z ++ "\n```"
        |> Ok
  in
  mergeGeneric mergeFn o a b


mergeTreeList : List (String, String) -> List (String, String) -> List (String, String) -> Result String (List (String, String))
mergeTreeList oList aList bList =
  let

    mergeFn x y z =
      Ok y
  in
  mergeGeneric mergeFn oList aList bList


treeToc : Dict String TreeObject -> String -> Dict String String
treeToc trees treeSha =
  let
    treeObject_ =
      Dict.get treeSha trees
  in
  case treeObject_ of
    Just tree ->
      tree.children -- List (String, String)
        |> List.map first -- List String
        |> List.map (treeToc trees) -- List (Dict String String)
        |> List.foldr Dict.union (Dict.fromList tree.children)-- Dict String String

    Nothing ->
      Dict.empty


commitToc : Dict String CommitObject -> Dict String TreeObject -> String -> Dict String String
commitToc commits trees commitSha =
  Dict.get commitSha commits -- Maybe CommitObject
    |> Maybe.map (\co -> treeToc trees co.tree) -- Maybe String
    |> Maybe.withDefault Dict.empty


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

