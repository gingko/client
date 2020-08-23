module Doc.Data exposing (Model, Objects, checkout, commitNew, defaultObjects, empty, success, toValue, update)

import Coders exposing (statusDecoder, tupleDecoder)
import Dict exposing (Dict)
import Diff3 exposing (diff3Merge)
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.TreeStructure exposing (apply, defaultTree, opToMsg)
import Doc.TreeUtils exposing (sha1)
import Json.Decode as Json
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe exposing (andThen)
import Time
import Tuple exposing (second)
import Types exposing (..)



-- MODEL


type Model
    = Model
        { refs : Dict String RefObject
        , commits : Dict String CommitObject
        , treeObjects : Dict String TreeObject
        , conflicts : List Conflict
        }


empty =
    Model { refs = Dict.empty, commits = Dict.empty, treeObjects = Dict.empty, conflicts = [] }


toValue : Model -> Enc.Value
toValue (Model model) =
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


type alias Objects =
    { commits : Dict String CommitObject
    , treeObjects : Dict String TreeObject
    , refs : Dict String RefObject
    }


defaultObjects : Objects
defaultObjects =
    Objects Dict.empty Dict.empty Dict.empty


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



-- UPDATE


update : Json.Value -> ( Model, Tree ) -> ( Model, Tree )
update json ( oldModel, oldTree ) =
    let
        refObjectDecoder =
            Json.map4 (\id v a r -> ( id, RefObject v a r ))
                (Json.field "_id" Json.string)
                (Json.field "value" Json.string)
                (Json.field "ancestors" (Json.list Json.string))
                (Json.field "_rev" Json.string)

        commitObjectDecoder =
            Json.map5 (\id t p a ts -> ( id, CommitObject t p a ts ))
                (Json.field "_id" Json.string)
                (Json.field "tree" Json.string)
                (Json.field "parents" (Json.list Json.string))
                (Json.field "author" Json.string)
                (Json.field "timestamp" Json.int)

        treeObjectDecoder =
            Json.map3 (\id cn ch -> ( id, TreeObject cn ch ))
                (Json.field "_id" Json.string)
                (Json.field "content" Json.string)
                (Json.field "children" (Json.list (tupleDecoder Json.string Json.string)))

        modelBuilder r c t =
            Model
                { refs = Dict.fromList r
                , commits = Dict.fromList c
                , treeObjects = Dict.fromList t
                , conflicts = [] -- TODO
                }

        dataDecoder =
            Json.map3 modelBuilder
                (Json.field "ref" (Json.list refObjectDecoder))
                (Json.field "commit" (Json.list commitObjectDecoder))
                (Json.field "tree" (Json.list treeObjectDecoder))
    in
    case Json.decodeValue dataDecoder json of
        Ok newModel ->
            ( newModel, checkoutRef "heads/master" newModel |> Maybe.withDefault oldTree )

        Err err ->
            let
                _ =
                    Debug.log "error" err
            in
            ( oldModel, oldTree )


checkoutRef : String -> Model -> Maybe Tree
checkoutRef refId (Model model) =
    Dict.get refId model.refs
        |> andThen (\ro -> Dict.get ro.value model.commits)
        |> andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")


success : Json.Value -> Model -> Model
success json (Model ({ refs } as model)) =
    let
        responseDecoder =
            Json.list
                (Json.map2 Tuple.pair
                    (Json.field "id" Json.string)
                    (Json.field "rev" Json.string)
                )
    in
    case Json.decodeValue responseDecoder json of
        Ok responses ->
            let
                updater ( id, newRev ) =
                    Dict.get id refs
                        |> Maybe.andThen (\r -> Just ( id, { r | rev = newRev } ))

                resDict =
                    responses
                        |> List.filterMap updater
                        |> Dict.fromList

                newRefs =
                    Dict.union resDict refs
            in
            Model { model | refs = newRefs }

        Err err ->
            Model model



-- GIT PORCELAIN


commitNew : String -> Int -> Tree -> Model -> Model
commitNew author timestamp tree ((Model { conflicts, refs, commits }) as model) =
    let
        localHead_ =
            Dict.get "heads/master" refs

        remoteHead_ =
            Dict.get "remotes/origin/master" refs
    in
    case ( localHead_, remoteHead_ ) of
        ( Nothing, Nothing ) ->
            -- Git Init (New document)
            commitTree author [] timestamp tree model

        ( Nothing, Just remoteHead ) ->
            -- Git Clone (Bare local). Disallow commit until local head is set.
            model

        ( Just localHead, Nothing ) ->
            -- Local document (unsynced changes).
            commitTree author [ localHead.value ] timestamp tree model

        ( Just localHead, Just remoteHead ) ->
            -- Local & Remote
            if List.isEmpty (List.filter (not << .resolved) conflicts) then
                -- No unresolved conflicts.
                commitTree author [ localHead.value, remoteHead.value ] timestamp tree model

            else
                -- Unresolved conflicts exist, dont' commit.
                model


checkout : String -> Objects -> ( Status, Maybe Tree )
checkout commitSha objects =
    ( Clean commitSha, checkoutCommit commitSha objects )



-- GIT PLUMBING


commitTree : String -> List String -> Int -> Tree -> Model -> Model
commitTree author parents timestamp tree (Model ({ refs, commits, treeObjects } as model)) =
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

        updateHead ref_ =
            let
                newRev =
                    case ref_ of
                        Just { rev } ->
                            rev

                        Nothing ->
                            ""
            in
            Just (RefObject newCommitSha [] newRev)
    in
    Model
        { model
            | refs = Dict.update "heads/master" updateHead refs
            , commits = Dict.insert newCommitSha newCommit commits
            , treeObjects = Dict.union treeObjects newTreeObjects
        }


checkoutCommit : String -> Objects -> Maybe Tree
checkoutCommit commitSha model =
    Dict.get commitSha model.commits
        |> andThen (\co -> treeObjectsToTree model.treeObjects co.tree "0")


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
generateCommitSha commitObj =
    (commitObj.tree ++ "\n")
        ++ (commitObj.parents |> String.join "\n")
        ++ (commitObj.author ++ " " ++ (commitObj.timestamp |> Debug.toString))
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


modelDecoder : Json.Decoder Objects
modelDecoder =
    Json.map3 Objects
        (Json.field "commits" commitsDecoder)
        (Json.field "treeObjects" treeObjectsDecoder)
        (Json.field "refs" refObjectsDecoder)


mergeDecoder : Json.Decoder ( Maybe String, Maybe String, Objects )
mergeDecoder =
    Json.map3 (\l r m -> ( l, r, m ))
        (Json.index 0 (Json.maybe Json.string))
        (Json.index 1 (Json.maybe Json.string))
        (Json.index 2 modelDecoder)


commitsToValue : Dict String CommitObject -> Enc.Value
commitsToValue commits =
    let
        commitToValue sha commitObj =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "type", Enc.string "commit" )
                , ( "tree", Enc.string commitObj.tree )
                , ( "parents", Enc.list Enc.string commitObj.parents )
                , ( "author", Enc.string commitObj.author )
                , ( "timestamp", Enc.int commitObj.timestamp )
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


changeDecoder : Objects -> Json.Decoder Objects
changeDecoder model =
    Json.oneOf
        [ Json.map3 Objects
            (Json.succeed model.commits)
            (Json.succeed model.treeObjects)
            refObjectsDecoder
        , Json.map3 Objects
            (Json.succeed model.commits)
            treeObjectsDecoder
            (Json.succeed model.refs)
        , Json.map3 Objects
            commitsDecoder
            (Json.succeed model.treeObjects)
            (Json.succeed model.refs)
        ]



-- HELPERS


conflict : Op -> Op -> Selection -> Conflict
conflict opA opB sel =
    Conflict "" opA opB sel False
        |> conflictWithSha
