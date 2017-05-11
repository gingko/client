module Objects exposing (Model, defaultModel, ObjMsg (Commit, Checkout, Merge), update, toValue)

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
  , refs : Dict String String
  }


defaultModel : Model
defaultModel = Model Dict.empty Dict.empty Dict.empty


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




-- GIT PORCELAIN

type ObjMsg
  = Commit (List String) String Tree
  | Checkout String
  | Merge Json.Value Tree


update : ObjMsg -> Model -> (Status, Maybe Tree, Model)
update msg model =
  case msg of
    Commit parents author tree ->
      let
        (newHead, newModel) =
          commitTree author parents tree model
            |> \(h, m) -> (h, updateRef "heads/master" h m)
      in
      (Clean newHead, Nothing, newModel)

    Checkout commitSha ->
      (Clean commitSha, checkoutCommit commitSha model, model)

    Merge json oldTree ->
      case Json.decodeValue modelDecoder json of
        Ok modelIn ->
          let
            _ = Debug.log "merge:modelIn" modelIn
            _ = Debug.log "merge:model" model

            oldHead_ = Dict.get "heads/master" model.refs
              |> Debug.log "merge:oldHead_"

            newHead_ = Dict.get "heads/master" modelIn.refs
              |> Debug.log "merge:newHead_"

            newModel =
              { model
                | treeObjects = Dict.union modelIn.treeObjects model.treeObjects
                , commits = Dict.union modelIn.commits model.commits
                , refs = Dict.union modelIn.refs model.refs
              }
          in
          case (oldHead_, newHead_) of
            (Just oldHead, Just newHead) ->
              merge oldHead newHead oldTree newModel
                |> Debug.log "merge:result"

            (Nothing, Just newHead) ->
              let
                newTree_ =
                  Dict.get newHead newModel.commits
                    |> andThen (\co -> treeObjectsToTree newModel.treeObjects co.tree "0")
              in
              (Clean newHead, newTree_, newModel)

            _ ->
              let _ = Debug.log "Error: no ref to master head commit." in
              (Bare, Nothing, model)

        Err err ->
          Debug.crash err




-- GIT PLUMBING

commitTree : String -> List String -> Tree -> Model -> (String, Model)
commitTree author parents tree model =
  let
    (newRootId, newTreeObjects) =
      writeTree tree

    newCommit = CommitObject
      newRootId
      parents
      author
      (timestamp ())

    newCommitSha = newCommit |> commitSha
  in
  ( newCommitSha
  , { model
      | commits = Dict.insert newCommitSha newCommit model.commits
      , treeObjects = Dict.union model.treeObjects newTreeObjects
    }
  )


checkoutCommit : String -> Model -> Maybe Tree
checkoutCommit commitSha model =
  Dict.get commitSha model.commits
    |> andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")


updateRef : String -> String -> Model -> Model
updateRef refId newValue model =
  { model
    | refs = model.refs
        |> Dict.insert refId newValue
  }


writeTree : Tree -> (String, Dict String TreeObject)
writeTree tree =
  case tree.children of
    Children treeList ->
      let
        (rootSha, rootTree) =
          treeToObject tree

        rootDict =
          (rootSha, rootTree)
            |> List.singleton
            |> Dict.fromList
      in
      ( rootSha
      , treeList
        |> List.map writeTree
        |> List.map second
        |> List.foldr Dict.union rootDict
      )


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

commitSha : CommitObject -> String
commitSha commit =
  ( commit.tree ++ "\n" ) ++
  ( commit.parents |> String.join "\n" ) ++
  ( commit.author ++ " " ++ ( commit.timestamp |> toString ) )
    |> sha1




-- ==== Merging

getCommonAncestor_ : Dict String CommitObject -> String -> String -> Maybe String
getCommonAncestor_ commits shaA shaB =
  let
    aAncestors = getAncestors commits shaA
    bAncestors = getAncestors commits shaB
  in
  aAncestors
    |> List.filter (\a -> List.member a bAncestors)
    |> Debug.log "commonAncestors"
    |> List.head


getAncestors : Dict String CommitObject -> String -> List String
getAncestors cm sh =
  let
    c_ = Dict.get sh cm
  in
  case c_ of
    Just c ->
      c.parents ++ (List.concatMap (getAncestors cm) c.parents)

    Nothing -> []


merge : String -> String -> Tree -> Model -> (Status, Maybe Tree, Model)
merge aSha bSha oldTree model =
  if (aSha == bSha || List.member bSha (getAncestors model.commits aSha)) then
    (Clean aSha, Just oldTree, model)
  else if (List.member aSha (getAncestors model.commits bSha)) then
    (Clean bSha, checkoutCommit bSha model, model)
  else
    let
      oSha = getCommonAncestor_ model.commits aSha bSha |> Maybe.withDefault ""
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
          (mTree, conflicts) = mergeTrees oTree aTree bTree
        in
        (MergeConflict mTree aSha bSha conflicts, Just mTree, model)

      _ ->
        Debug.crash "failed merge"
        --(MergeConflict aSha bSha [], Nothing, model)


mergeTrees : Tree -> Tree -> Tree -> (Tree, List Conflict)
mergeTrees oTree aTree bTree =
  let
    (mContent, contentConflicts) = mergeStrings oTree.id oTree.content aTree.content bTree.content

    (mChildren, childrenConflicts) = mergeChildren (getChildren oTree) (getChildren aTree) (getChildren bTree)
  in
  (Tree oTree.id mContent (Children mChildren), contentConflicts ++ childrenConflicts)

type MergeColumn = O | A | B
type alias MergeDict = Dict String (Maybe Tree, Maybe Tree, Maybe Tree)


mergeChildren : List Tree -> List Tree -> List Tree -> (List Tree, List Conflict)
mergeChildren oList aList bList =
  let
    allTrees = oList ++ aList ++ bList
      |> Debug.log "merge:allTrees"

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
      |> Debug.log "merge:allVals"

    mergeResults =
      allVals
        |> Dict.toList -- List (String, (MbT, MbT, MbT))
        |> List.filterMap (\(id, (o_, a_, b_)) ->
            case (o_, a_, b_) of
              (Just ot, Just at, Just bt) -> -- Present in all trees
                Just (mergeTrees ot at bt)

              (Just ot, Just at, Nothing) -> -- Deleted on b
                if ot == at then
                  Nothing
                else                         -- modify/delete conflict
                  Just (ot, [Conflict ot.id (Mod at.content) Del Ours False])

              (Just ot, Nothing, Just bt) -> -- Deleted on a
                if ot == bt then
                  Nothing
                else                         -- delete/modify conflict
                  Just (ot, [Conflict ot.id Del (Mod bt.content) Ours False])

              (Nothing, Nothing, Just bt) -> -- Added on b only
                Just (bt, [])

              (Nothing, Just at, Nothing) -> -- Added on a only
                Just (at, [])

              (Nothing, Just at, Just bt) -> -- Added on a and b. Via move?
                if at == bt then
                  Just (at, []) -- TODO: move/move conflict
                else
                  Just (at, []) -- TODO: modify&move/modify&move conflict?

              _ ->
                Debug.crash "impossible state?"

          ) -- List (Tree, List Conflict)
        |> \mr -> (List.map first mr, List.concatMap second mr)
  in
  mergeResults


getTreeDict : MergeColumn -> List Tree -> MergeDict
getTreeDict col trees =
  case col of
    O -> trees |> List.map (\t -> (t.id, (Just t, Nothing, Nothing)) ) |> Dict.fromList
    A -> trees |> List.map (\t -> (t.id, (Nothing, Just t, Nothing)) ) |> Dict.fromList
    B -> trees |> List.map (\t -> (t.id, (Nothing, Nothing, Just t)) ) |> Dict.fromList



mergeStrings : String -> String -> String -> String -> (String, List Conflict)
mergeStrings id o a b =
  let
    mergeFn x y z =
      (x, [Conflict id (Mod y) (Mod z) Ours False])
  in
  mergeGeneric mergeFn o a b


mergeTreeList : List (String, String) -> List (String, String) -> List (String, String) -> (List (String, String), List Conflict)
mergeTreeList oList aList bList =
  let
    mergeFn x y z =
      (x, [])
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


mergeGeneric : (a -> a -> a -> (a, List Conflict)) -> a -> a -> a -> (a, List Conflict)
mergeGeneric mergeFn o a b =
  if a == b then
    (a, [])
  else if o == b && o /= a then
    (a, [])
  else if o == a && o /= b then
    (b, [])
  else
    mergeFn o a b




-- PORTS & INTEROP

toValue : Model -> Enc.Value
toValue model =
  let
    treeObjectToValue sha treeObject =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "tree" )
        , ( "content", Enc.string treeObject.content )
        , ( "children", Enc.list
              (List.map (\(s, i) -> Enc.list [Enc.string s, Enc.string i]) treeObject.children) )
        ]

    refToValue sha refString =
      Enc.object
        [ ( "_id", Enc.string sha )
        , ( "type", Enc.string "ref" )
        , ( "value", Enc.string refString )
        ]

    commits =
      commitsToValue model.commits

    treeObjects =
      Dict.toList model.treeObjects
        |> List.map (\(k, v) -> treeObjectToValue k v)
        |> Enc.list

    refs =
      Dict.toList model.refs
        |> List.map (\(k, v) -> refToValue k v)
        |> Enc.list
  in
  Enc.object
    [ ( "commits", commits )
    , ( "treeObjects", treeObjects )
    , ( "refs", refs )
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
    ( Json.field "refs" (Json.dict Json.string))


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


refDecoder : Json.Decoder String
refDecoder =
    ( Json.field "value" Json.string )


changeDecoder : Model -> Json.Decoder Model
changeDecoder model =
  Json.oneOf
    [ Json.map3 Model
        ( Json.succeed model.commits )
        ( Json.succeed model.treeObjects )
        ( Json.dict refDecoder )
    , Json.map3 Model
        ( Json.succeed model.commits )
        treeObjectsDecoder
        ( Json.succeed model.refs )
    , Json.map3 Model
        commitsDecoder
        ( Json.succeed model.treeObjects )
        ( Json.succeed model.refs )
    ]
