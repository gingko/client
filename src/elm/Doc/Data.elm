module Doc.Data exposing (Model, Msg(..), defaultModel, init, setHeadRev, toValue, update)

import Coders exposing (metadataDecoder, statusDecoder, tripleDecoder, tupleDecoder)
import Dict exposing (Dict)
import Diff3 exposing (diff3Merge)
import Doc.TreeStructure exposing (apply, opToMsg)
import Doc.TreeUtils exposing (sha1)
import Json.Decode as Json
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe exposing (andThen)
import Tuple exposing (first, second)
import Types exposing (..)



-- MODEL


type alias Model =
    { commits : Dict String CommitObject
    , treeObjects : Dict String TreeObject
    , refs : Dict String RefObject
    }


defaultModel : Model
defaultModel =
    Model Dict.empty Dict.empty Dict.empty


type alias TreeObject =
    { content : String
    , children : List ( String, String ) -- List (sha, tree id)
    }


type alias CommitObject =
    { tree : String
    , parents : List String
    , author : String
    , timestamp : Int
    }


type alias RefObject =
    { value : String
    , ancestors : List String
    , rev : String
    }


type alias DocumentData =
    { metadata : ( Maybe String, String )
    , status : Status
    , builtTree : Maybe Tree
    , objects : Model
    }


init : Json.Value -> DocumentData
init json =
    case Json.decodeValue (tripleDecoder metadataDecoder statusDecoder modelDecoder) json of
        Ok ( metadata, status, modelIn ) ->
            case status of
                MergeConflict mTree _ _ _ ->
                    DocumentData metadata status (Just mTree) modelIn

                Clean sha ->
                    let
                        newTree_ =
                            Dict.get sha modelIn.commits
                                |> andThen (\co -> treeObjectsToTree modelIn.treeObjects co.tree "0")
                    in
                    DocumentData metadata (Clean sha) newTree_ modelIn

                Bare ->
                    DocumentData metadata Bare Nothing modelIn

        Err err ->
            Debug.todo ("Data.Init:" ++ Json.errorToString err)



-- GIT PORCELAIN


type Msg
    = Commit (List String) String Int Tree
    | Checkout String
    | Merge Json.Value Tree


update : Msg -> Model -> ( Status, Maybe Tree, Model )
update msg model =
    case msg of
        Commit parents author timestamp tree ->
            let
                ( newHead, newModel ) =
                    commitTree author parents timestamp tree model
                        |> (\( h, m ) -> ( h, updateRef "heads/master" h m ))
            in
            ( Clean newHead, Nothing, newModel )

        Checkout commitSha ->
            ( Clean commitSha, checkoutCommit commitSha model, model )

        Merge json oldTree ->
            case Json.decodeValue mergeDecoder json of
                Ok ( localHead_, remoteHead_, modelIn ) ->
                    let
                        newModel =
                            { model
                                | treeObjects = Dict.union modelIn.treeObjects model.treeObjects
                                , commits = Dict.union modelIn.commits model.commits
                                , refs = Dict.union modelIn.refs model.refs
                            }
                    in
                    case ( localHead_, remoteHead_ ) of
                        ( Just localHead, Just remoteHead ) ->
                            merge localHead remoteHead oldTree newModel

                        ( Nothing, Just remoteHead ) ->
                            let
                                newTree_ =
                                    Dict.get remoteHead newModel.commits
                                        |> andThen (\co -> treeObjectsToTree newModel.treeObjects co.tree "0")
                            in
                            ( Clean remoteHead, newTree_, newModel )

                        _ ->
                            let
                                _ =
                                    Debug.log "Error: no ref to master head commit."
                            in
                            ( Bare, Nothing, model )

                Err err ->
                    Debug.todo (Json.errorToString err)



-- GIT PLUMBING


commitTree : String -> List String -> Int -> Tree -> Model -> ( String, Model )
commitTree author parents timestamp tree model =
    let
        ( newRootId, newTreeObjects ) =
            writeTree tree

        newCommit =
            CommitObject
                newRootId
                parents
                author
                timestamp

        newCommitSha =
            newCommit |> generateCommitSha
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
        | refs =
            model.refs
                |> Dict.update refId
                    (\mbr ->
                        case mbr of
                            Just { value, ancestors, rev } ->
                                Just (RefObject newValue (value :: ancestors) rev)

                            Nothing ->
                                Just (RefObject newValue [] "")
                    )
    }


setHeadRev : String -> Model -> Model
setHeadRev newRev model =
    { model
        | refs =
            model.refs
                |> Dict.update "heads/master"
                    (\mbr ->
                        case mbr of
                            Just { value, ancestors, rev } ->
                                Just (RefObject value ancestors newRev)

                            Nothing ->
                                Just (RefObject "" [] newRev)
                    )
    }


writeTree : Tree -> ( String, Dict String TreeObject )
writeTree tree =
    case tree.children of
        Children treeList ->
            let
                ( rootSha, rootTree ) =
                    treeToObject tree

                rootDict =
                    ( rootSha, rootTree )
                        |> List.singleton
                        |> Dict.fromList
            in
            ( rootSha
            , treeList
                |> List.map writeTree
                |> List.map second
                |> List.foldr Dict.union rootDict
            )


treeToObject : Tree -> ( String, TreeObject )
treeToObject tree =
    case treeToObjectId tree of
        ( sha, id, treeObj ) ->
            ( sha, treeObj )


treeToObjectId : Tree -> ( String, String, TreeObject )
treeToObjectId { id, content, children } =
    case children of
        Children [] ->
            ( content ++ "\n" |> sha1, id, TreeObject content [] )

        Children treeList ->
            let
                childrenIds =
                    treeList
                        |> List.map treeToObjectId
                        |> List.map (\( tid, u, obj ) -> ( tid, u ))
            in
            ( content
                ++ "\n"
                ++ (childrenIds
                        |> List.map (\( i, u ) -> i ++ " " ++ u)
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
        Just { content, children } ->
            let
                fMap ( sh, i ) =
                    treeObjectsToTree treeObjects sh i

                subtrees =
                    children
                        |> List.filterMap fMap
                        -- List Tree
                        |> Children
            in
            Just (Tree id content subtrees)

        Nothing ->
            Nothing


generateCommitSha : CommitObject -> String
generateCommitSha commit =
    (commit.tree ++ "\n")
        ++ (commit.parents |> String.join "\n")
        ++ (commit.author ++ " " ++ (commit.timestamp |> Debug.toString))
        |> sha1


conflictWithSha : Conflict -> Conflict
conflictWithSha { id, opA, opB, selection, resolved } =
    Conflict
        (String.join "\n" [ Debug.toString opA, Debug.toString opB, Debug.toString selection, Debug.toString resolved ] |> sha1)
        opA
        opB
        selection
        resolved



-- ==== Merging


merge : String -> String -> Tree -> Model -> ( Status, Maybe Tree, Model )
merge aSha bSha oldTree model =
    if aSha == bSha || List.member bSha (getAncestors model.commits aSha) then
        ( Clean aSha, Just oldTree, model )

    else if List.member aSha (getAncestors model.commits bSha) then
        ( Clean bSha, checkoutCommit bSha model, model )

    else
        let
            oSha =
                getCommonAncestor_ model.commits aSha bSha |> Maybe.withDefault ""

            getTree_ sha =
                Dict.get sha model.commits
                    |> Maybe.andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")

            oTree_ =
                getTree_ oSha

            aTree_ =
                getTree_ aSha

            bTree_ =
                getTree_ bSha
        in
        case ( oTree_, aTree_, bTree_ ) of
            ( Just oTree, Just aTree, Just bTree ) ->
                let
                    ( mTree, conflicts ) =
                        mergeTreeStructure oTree aTree bTree
                in
                ( MergeConflict mTree aSha bSha conflicts, Just mTree, model )

            ( Nothing, Just _, Just _ ) ->
                Debug.todo "failed merge, no common ancestor found."

            _ ->
                Debug.todo "failed merge"



--(MergeConflict aSha bSha [], Nothing, model)


mergeTreeStructure : Tree -> Tree -> Tree -> ( Tree, List Conflict )
mergeTreeStructure oTree aTree bTree =
    let
        ( cleanOps, conflicts ) =
            getConflicts (getOps oTree aTree |> Debug.log "aTree ops") (getOps oTree bTree |> Debug.log "bTree ops")
    in
    ( treeFromOps oTree cleanOps, conflicts )


treeFromOps : Tree -> List Op -> Tree
treeFromOps oTree ops =
    oTree
        |> apply (List.map (opToMsg oTree) ops)


getTreePaths : Tree -> Dict String ( String, List String, Int )
getTreePaths tree =
    getTreePathsWithParents [] 0 tree


getTreePathsWithParents : List String -> Int -> Tree -> Dict String ( String, List String, Int )
getTreePathsWithParents parents idx tree =
    let
        rootDict =
            Dict.empty
                |> Dict.insert tree.id ( tree.content, parents, idx )
    in
    case tree.children of
        Children [] ->
            rootDict

        Children children ->
            children
                |> List.indexedMap (getTreePathsWithParents (parents ++ [ tree.id ]))
                |> List.foldl Dict.union Dict.empty
                |> Dict.union rootDict


getOps : Tree -> Tree -> List Op
getOps oldTree newTree =
    let
        oPaths =
            getTreePaths oldTree

        nPaths =
            getTreePaths newTree

        oldOnly : String -> ( String, List String, Int ) -> List Op -> List Op
        oldOnly id ( _, parents, _ ) ops =
            ops ++ [ Del id parents ]

        newOnly : String -> ( String, List String, Int ) -> List Op -> List Op
        newOnly id ( content, parents, _ ) ops =
            ops ++ [ Ins id content parents 0 ]

        both : String -> ( String, List String, Int ) -> ( String, List String, Int ) -> List Op -> List Op
        both id ( oldContent, oldParents, oldIdx ) ( newContent, newParents, newIdx ) ops =
            let
                modOp =
                    if oldContent /= newContent then
                        [ Mod id oldParents newContent oldContent ]

                    else
                        []

                movOp =
                    if (oldParents /= newParents) || (oldIdx /= newIdx) then
                        [ Mov id oldParents oldIdx newParents newIdx ]

                    else
                        []
            in
            ops ++ modOp

        ignoreOp : Tree -> Op -> Op -> Bool
        ignoreOp oTree op1 op2 =
            case ( op1, op2 ) of
                ( Del id1 parents1, Del id2 parents2 ) ->
                    if List.member id2 parents1 then
                        True

                    else
                        False

                _ ->
                    False

        maybeIgnore : Tree -> ( Op, List Op ) -> Maybe Op
        maybeIgnore oTree ( newOp, ops ) =
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
                |> ListExtra.select
                -- List (Op, List Op)
                |> List.filterMap (maybeIgnore oTree)
    in
    Dict.merge oldOnly both newOnly oPaths nPaths []
        |> collapseDelOps oldTree


getConflicts : List Op -> List Op -> ( List Op, List Conflict )
getConflicts opsA opsB =
    let
        liftFn : Op -> Op -> ( List Op, List Conflict )
        liftFn opA opB =
            case ( opA, opB ) of
                -- Modify/Modify conflict
                ( Mod idA pidsA strA orig, Mod idB pidsB strB _ ) ->
                    if idA == idB && strA /= strB then
                        case diff3Merge (String.lines strA) (String.lines orig) (String.lines strB) of
                            [ Diff3.DiffOk mergedStrings ] ->
                                ( [ Mod idA pidsA (mergedStrings |> String.join "\n") orig ], [] )

                            _ ->
                                ( [], [ conflict opA opB Manual ] )

                    else
                        ( [ opA, opB ], [] )

                -- Modify/Delete conflicts
                ( Mod idA pidsA strA _, Del idB _ ) ->
                    if idA == idB || List.member idB pidsA then
                        ( [], [ conflict opA opB Ours ] )

                    else
                        ( [ opA, opB ], [] )

                ( Del idA _, Mod idB pidsB strB _ ) ->
                    if idA == idB || List.member idA pidsB then
                        ( [], [ conflict opA opB Theirs ] )

                    else
                        ( [ opA, opB ], [] )

                -- Insert/Delete conflicts
                ( Ins idA _ pidsA _, Del idB _ ) ->
                    if idA == idB || List.member idB pidsA then
                        ( [], [ conflict opA opB Ours ] )

                    else
                        ( [ opA, opB ], [] )

                ( Del idA _, Ins idB _ pidsB _ ) ->
                    if idA == idB || List.member idA pidsB then
                        ( [], [ conflict opA opB Theirs ] )

                    else
                        ( [ opA, opB ], [] )

                ( Mov idA oldParentsA oldIdxA newParentsA newIdxA, Mov idB oldParentsB oldIdxB newParentsB newIdxB ) ->
                    if areAcyclicMoves ( idA, newParentsA ) ( idB, newParentsB ) then
                        ( [], [ conflict opA opB Ours ] )

                    else
                        ( [ opA, opB ], [] )

                _ ->
                    ( [ opA, opB ], [] )
    in
    ListExtra.lift2 liftFn opsA opsB
        -- List (List Op, List Conflict)
        |> List.foldl
            (\( os, cs ) ( osAcc, csAcc ) -> ( osAcc ++ os, csAcc ++ cs ))
            ( [], [] )
        |> (\( os, cs ) -> ( os |> ListExtra.uniqueBy Debug.toString, cs |> ListExtra.uniqueBy Debug.toString ))



-- Hacky way to remove duplicate Ops


areAcyclicMoves : ( String, List String ) -> ( String, List String ) -> Bool
areAcyclicMoves ( idA, pidsA ) ( idB, pidsB ) =
    List.member idA pidsB || List.member idB pidsA


getCommonAncestor_ : Dict String CommitObject -> String -> String -> Maybe String
getCommonAncestor_ commits shaA shaB =
    let
        aAncestors =
            getAncestors commits shaA

        bAncestors =
            getAncestors commits shaB
    in
    aAncestors
        |> List.filter (\a -> List.member a bAncestors)
        |> List.head


getAncestors : Dict String CommitObject -> String -> List String
getAncestors cm sh =
    let
        c_ =
            Dict.get sh cm
    in
    case c_ of
        Just c ->
            c.parents ++ List.concatMap (getAncestors cm) c.parents

        Nothing ->
            []



-- PORTS & INTEROP


toValue : Model -> Enc.Value
toValue model =
    let
        treeObjectToValue sha treeObject =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "type", Enc.string "tree" )
                , ( "content", Enc.string treeObject.content )
                , ( "children"
                  , Enc.list (Enc.list Enc.string) (List.map (\( childSha, childId ) -> [ childSha, childId ]) treeObject.children)
                  )
                ]

        refToValue sha ref =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "_rev", Enc.string ref.rev )
                , ( "type", Enc.string "ref" )
                , ( "value", Enc.string ref.value )
                , ( "ancestors", Enc.list Enc.string ref.ancestors )
                ]

        commits =
            commitsToValue model.commits

        treeObjects =
            Dict.toList model.treeObjects
                |> List.map (\( k, v ) -> treeObjectToValue k v)
                |> Enc.list identity

        refs =
            Dict.toList model.refs
                |> List.map (\( k, v ) -> refToValue k v)
                |> Enc.list identity
    in
    Enc.object
        [ ( "commits", commits )
        , ( "treeObjects", treeObjects )
        , ( "refs", refs )
        ]


modelDecoder : Json.Decoder Model
modelDecoder =
    Json.map3 Model
        (Json.field "commits" commitsDecoder)
        (Json.field "treeObjects" treeObjectsDecoder)
        (Json.field "refs" refObjectsDecoder)


mergeDecoder : Json.Decoder ( Maybe String, Maybe String, Model )
mergeDecoder =
    Json.map3 (\l r m -> ( l, r, m ))
        (Json.index 0 (Json.maybe Json.string))
        (Json.index 1 (Json.maybe Json.string))
        (Json.index 2 modelDecoder)


commitsToValue : Dict String CommitObject -> Enc.Value
commitsToValue commits =
    let
        commitToValue sha commit =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "type", Enc.string "commit" )
                , ( "tree", Enc.string commit.tree )
                , ( "parents", Enc.list Enc.string commit.parents )
                , ( "author", Enc.string commit.author )
                , ( "timestamp", Enc.int commit.timestamp )
                ]
    in
    Dict.toList commits
        |> List.map (\( k, v ) -> commitToValue k v)
        |> Enc.list identity


commitsDecoder : Json.Decoder (Dict String CommitObject)
commitsDecoder =
    let
        commitObjectDecoder : Json.Decoder CommitObject
        commitObjectDecoder =
            Json.map4 CommitObject
                (Json.field "tree" Json.string)
                (Json.field "parents" (Json.list Json.string))
                (Json.field "author" Json.string)
                (Json.field "timestamp" Json.int)
    in
    Json.dict commitObjectDecoder


treeObjectsDecoder : Json.Decoder (Dict String TreeObject)
treeObjectsDecoder =
    let
        tupleDecoder a b =
            Json.index 0 a
                |> Json.andThen
                    (\aVal ->
                        Json.index 1 b
                            |> Json.andThen (\bVal -> Json.succeed ( aVal, bVal ))
                    )

        treeObjectDecoder =
            Json.map2 TreeObject
                (Json.field "content" Json.string)
                (Json.field "children" (Json.list (tupleDecoder Json.string Json.string)))
    in
    Json.dict treeObjectDecoder


refObjectsDecoder : Json.Decoder (Dict String RefObject)
refObjectsDecoder =
    let
        refObjectDecoder =
            Json.map3 RefObject
                (Json.field "value" Json.string)
                (Json.field "ancestors" (Json.list Json.string))
                (Json.field "_rev" Json.string)
    in
    Json.dict refObjectDecoder


changeDecoder : Model -> Json.Decoder Model
changeDecoder model =
    Json.oneOf
        [ Json.map3 Model
            (Json.succeed model.commits)
            (Json.succeed model.treeObjects)
            refObjectsDecoder
        , Json.map3 Model
            (Json.succeed model.commits)
            treeObjectsDecoder
            (Json.succeed model.refs)
        , Json.map3 Model
            commitsDecoder
            (Json.succeed model.treeObjects)
            (Json.succeed model.refs)
        ]



-- HELPERS


conflict : Op -> Op -> Selection -> Conflict
conflict opA opB sel =
    Conflict "" opA opB sel False
        |> conflictWithSha
