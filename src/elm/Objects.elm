module Objects exposing (Model, defaultModel, ObjMsg (Commit, Checkout, Init, Merge), update, toValue)

import Dict exposing (Dict)
import Maybe exposing (andThen)
import Tuple exposing (first, second)
import List.Extra as ListExtra

import Json.Encode as Enc
import Json.Decode as Json
import Json.Decode.Pipeline exposing (decode, required, optional)
import Coders exposing (statusDecoder, tupleDecoder)

import Types exposing (..)
import Trees exposing (apply, opToTreeMsg)
import TreeUtils exposing (getChildren, getTree)
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
  | Init Json.Value
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

    Init json ->
      case Json.decodeValue (tupleDecoder statusDecoder modelDecoder) json of
        Ok (status, modelIn) ->
          case status of
            MergeConflict mTree _ _ _ ->
              (status, Just mTree, modelIn)

            Clean sha ->
              let
                newTree_ =
                  Dict.get sha modelIn.commits
                    |> andThen (\co -> treeObjectsToTree modelIn.treeObjects co.tree "0")
              in
              (Clean sha, newTree_, modelIn)

            Bare ->
              (Bare, Nothing, modelIn)

        Err err ->
          Debug.crash ("Objects.Init:" ++ err)

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


conflictWithSha : Conflict -> Conflict
conflictWithSha {id, opA, opB, selection, resolved} =
  Conflict
    (String.join "\n" [toString opA, toString opB, toString selection, toString resolved] |> sha1)
    opA opB selection resolved




-- ==== Merging

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
    (cleanOps, conflicts) = getConflicts (getOps oTree aTree) (getOps oTree bTree)
      |> Debug.log "conflicts"
  in
  (treeFromOps oTree cleanOps, conflicts)


treeFromOps : Tree -> List Op -> Tree
treeFromOps oTree ops =
  oTree
    |> Debug.log "conflicts:oTree"
    |> apply (List.map opToTreeMsg ops)
    |> Debug.log "conflicts:treeFromOps"


getTreePaths : Tree -> Dict String (String, List String)
getTreePaths tree =
  getTreePathsWithParents [] tree


getTreePathsWithParents : List String -> Tree -> Dict String (String, List String)
getTreePathsWithParents parents tree =
  let
    rootDict = Dict.empty
      |> Dict.insert tree.id (tree.content, parents)
  in
  case tree.children of
    Children [] ->
      rootDict

    Children children ->
      children
        |> List.map (getTreePathsWithParents (parents ++ [tree.id]))
        |> List.foldl Dict.union Dict.empty
        |> Dict.union rootDict


getOps : Tree -> Tree -> List Op
getOps oldTree newTree =
  let
    oPaths = getTreePaths oldTree |> Debug.log "paths:oPaths"
    nPaths = getTreePaths newTree |> Debug.log "paths:nPaths"

    oldOnly : String -> (String, List String) -> List Op -> List Op
    oldOnly id (content, parents) ops =
      ops ++ [Del id parents]

    newOnly : String -> (String, List String) -> List Op -> List Op
    newOnly id (content, parents) ops =
      let
        insTree_ = getTree id newTree
      in
      case insTree_ of
        Just insTree ->
          ops ++ [Ins insTree parents 0]

        _ ->
          ops

    both : String -> (String, List String) -> (String, List String) -> List Op -> List Op
    both id (oldContent, oldParents) (newContent, newParents) ops =
      let
        modOp =
          if oldContent /= newContent then
            [Mod id oldParents newContent]
          else
            []

        movOp = [] -- TODO: oldParents /= newParents then Mov
      in
      ops ++ modOp ++ movOp

    ignoreOp : Tree -> Op -> Op -> Bool
    ignoreOp oTree op1 op2 =
      case (op1, op2) of
        (Del id1 parents1, Del id2 parents2) ->
            if (List.member id2 parents1) then
              True
            else
              False

        _ ->
          False

    maybeIgnore : Tree -> (Op , List Op) -> Maybe Op
    maybeIgnore oTree (newOp, ops) =
      let
        ignore =
          ops
            |> List.map (ignoreOp oTree newOp)
            |> List.any identity
      in
      if ignore then
        Nothing
      else
        Just newOp

    collapseDelOps : Tree -> List Op -> List Op
    collapseDelOps oTree ops =
      ops
        |> ListExtra.select -- List (Op, List Op)
        |> List.filterMap (maybeIgnore oTree)

  in
  Dict.merge oldOnly both newOnly oPaths nPaths []
    |> collapseDelOps oldTree


getConflicts : List Op -> List Op -> (List Op, List Conflict)
getConflicts opsA opsB =
  let
    liftFn : Op -> Op -> (List Op, List Conflict)
    liftFn opA opB =
      case (opA, opB) of
        -- Modify/Modify conflict
        (Mod idA pidsA strA, Mod idB pidsB strB) ->
          if idA == idB && strA /= strB then
            ([], [conflict opA opB])
          else
            ([opA, opB], [])

        -- Modify/Delete conflicts
        (Mod idA pidsA strA, Del idB _) ->
          if idA == idB || (List.member idB pidsA) then
            ([], [conflict opA opB])
          else
            ([opA, opB], [])
        (Del idA _, Mod idB pidsB strB) ->
          if idA == idB || (List.member idA pidsB) then
            ([], [conflict opA opB])
          else
            ([opA, opB], [])

        -- Insert/Delete conflicts
        (Ins {id} pidsA _, Del idB _) ->
          if id == idB || (List.member idB pidsA) then
            ([],[conflict opA opB])
          else
            ([opA, opB], [])
        (Del idA _, Ins {id} pidsB _) ->
          if idA == id || (List.member idA pidsB) then
            ([],[conflict opA opB])
          else
            ([opA, opB], [])

        -- TODO: Move Conflicts
        _ ->
          ([opA, opB], [])

  in
  ListExtra.lift2 liftFn opsA opsB -- List (List Op, List Conflict)
    |> List.foldl
        (\(os, cs) (osAcc, csAcc) -> (osAcc ++ os, csAcc ++ cs))
        ([], [])


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


-- HELPERS

conflict : Op -> Op -> Conflict
conflict opA opB =
  Conflict "" opA opB Ours False
    |> conflictWithSha
