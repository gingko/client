module Doc.Data exposing (DataCmd(..), Model, Objects, checkout, commitNew, defaultObjects, empty, encode, input, success)

import Coders exposing (statusDecoder, tupleDecoder)
import Dict exposing (Dict)
import Diff3 exposing (diff3Merge)
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.TreeStructure exposing (apply, defaultTree, opToMsg)
import Doc.TreeUtils exposing (sha1)
import Json.Decode as Dec
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe exposing (andThen)
import Time
import Tuple exposing (second)
import Types exposing (..)



-- MODEL


type Model
    = Model Data


type alias Data =
    { refs : Dict String RefObject
    , commits : Dict String CommitObject
    , treeObjects : Dict String TreeObject
    , conflicts : List Conflict
    }


empty : Model
empty =
    Model
        { refs = Dict.empty
        , commits = Dict.empty
        , treeObjects = Dict.empty
        , conflicts = []
        }


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



-- EXPOSED FUNCTIONS


type DataCmd
    = None
    | SendPush
    | SendSave


input : Dec.Value -> ( Model, Tree ) -> ( Model, Tree, DataCmd )
input json ( Model oldData, oldTree ) =
    case Dec.decodeValue decode json of
        Ok (Model newData) ->
            let
                headUpdater refHead ro_ =
                    case ro_ of
                        Nothing ->
                            Just (RefObject refHead.value [] "")

                        Just ro ->
                            Just { ro | value = refHead.value }

                newStatus =
                    getStatus newData |> Debug.log "newStatus"
            in
            case newStatus of
                LocalOnly _ ->
                    ( Model newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree, SendPush )

                RemoteOnly remoteHead ->
                    let
                        updatedData =
                            { newData | refs = Dict.update "heads/master" (headUpdater remoteHead) newData.refs }
                    in
                    ( Model updatedData, checkoutRef "heads/master" updatedData |> Maybe.withDefault oldTree, SendSave )

                LocalAhead _ ->
                    ( Model newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree, SendPush )

                RemoteAhead remoteHead ->
                    let
                        updatedData =
                            { newData | refs = Dict.update "heads/master" (headUpdater remoteHead) newData.refs }
                    in
                    ( Model newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree, SendSave )

                InSync _ _ ->
                    ( Model newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree, None )

                _ ->
                    ( Model newData, oldTree, None )

        {-

           let
               localHead_ =
                   Dict.get "heads/master" oldData.refs

               remoteHead_ =
                   Dict.get "remotes/origin/master" refs
           in
           case ( localHead_, remoteHead_ ) of
               ( Nothing, Just remoteHead ) ->
                   -- Git Clone. Set new local head to be same as remote.
                   ( Model { newData | refs = Dict.insert "heads/master" remoteHead refs }, oldTree )

               ( Just localHead, Nothing ) ->
                   -- Git Init. Not pushed to remote yet.
                   ( Model newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree )

               ( Just localHead, Just remoteHead ) ->
                   -- Git Pull. Merge changes
                   ( Model oldData, oldTree )

               ( Nothing, Nothing ) ->
                   ( Model oldData, oldTree )
        -}
        Err err ->
            let
                _ =
                    Debug.log "error" err
            in
            ( Model oldData, oldTree, None )


checkoutRef : String -> Data -> Maybe Tree
checkoutRef refId data =
    Dict.get refId data.refs
        |> andThen (\ro -> Dict.get ro.value data.commits)
        |> andThen (\co -> treeObjectsToTree data.treeObjects co.tree "0")


success : Dec.Value -> Model -> Model
success json (Model ({ refs } as model)) =
    let
        responseDecoder =
            Dec.list
                (Dec.map2 Tuple.pair
                    (Dec.field "id" Dec.string)
                    (Dec.field "rev" Dec.string)
                )
    in
    case Dec.decodeValue responseDecoder json of
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


commitNew : String -> Int -> Tree -> Model -> Model
commitNew author timestamp tree ((Model ({ conflicts, refs, commits } as data)) as model) =
    case getStatus data of
        Empty ->
            commitTree author [] timestamp tree model

        LocalOnly localHead ->
            commitTree author [ localHead.value ] timestamp tree model

        RemoteOnly _ ->
            model

        LocalAhead localHead ->
            commitTree author [ localHead.value ] timestamp tree model

        RemoteAhead remoteHead ->
            commitTree author [ remoteHead.value ] timestamp tree model

        Merging localHead remoteHead ->
            commitTree author [ localHead.value, remoteHead.value ] timestamp tree model

        InSync localHead _ ->
            commitTree author [ localHead.value ] timestamp tree model



{-
   InConflict ->
       -- Local & Remote
       if List.isEmpty (List.filter (not << .resolved) conflicts) then
           -- No unresolved conflicts.
           commitTree author [ localHead.value, remoteHead.value ] timestamp tree model

       else
           -- Unresolved conflicts exist, dont' commit.
           model

-}


checkout : String -> Objects -> ( Status, Maybe Tree )
checkout commitSha objects =
    ( Clean commitSha, checkoutCommit commitSha objects )



-- INTERNALS


type MyStatus
    = Empty
    | LocalOnly RefObject
    | RemoteOnly RefObject
    | LocalAhead RefObject
    | RemoteAhead RefObject
    | Merging RefObject RefObject
    | InSync RefObject RefObject


getStatus : Data -> MyStatus
getStatus { refs, commits } =
    let
        localHead_ =
            Dict.get "heads/master" refs

        remoteHead_ =
            Dict.get "remotes/origin/master" refs
    in
    case ( localHead_, remoteHead_ ) of
        ( Nothing, Nothing ) ->
            Empty

        ( Just localHead, Nothing ) ->
            LocalOnly localHead

        ( Nothing, Just remoteHead ) ->
            RemoteOnly remoteHead

        ( Just localHead, Just remoteHead ) ->
            if localHead.value == remoteHead.value then
                InSync localHead remoteHead

            else if List.member localHead.value (getAncestors commits remoteHead.value) then
                RemoteAhead remoteHead

            else if List.member remoteHead.value (getAncestors commits localHead.value) then
                LocalAhead localHead

            else
                -- Merge Conflict of some sort
                Merging localHead remoteHead


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


decode : Dec.Decoder Model
decode =
    let
        refObjectDecoder =
            Dec.map4 (\id v a r -> ( id, RefObject v a r ))
                (Dec.field "_id" Dec.string)
                (Dec.field "value" Dec.string)
                (Dec.field "ancestors" (Dec.list Dec.string))
                (Dec.field "_rev" Dec.string)

        commitObjectDecoder =
            Dec.map5 (\id t p a ts -> ( id, CommitObject t p a ts ))
                (Dec.field "_id" Dec.string)
                (Dec.field "tree" Dec.string)
                (Dec.field "parents" (Dec.list Dec.string))
                (Dec.field "author" Dec.string)
                (Dec.field "timestamp" Dec.int)

        treeObjectDecoder =
            Dec.map3 (\id cn ch -> ( id, TreeObject cn ch ))
                (Dec.field "_id" Dec.string)
                (Dec.field "content" Dec.string)
                (Dec.field "children" (Dec.list (tupleDecoder Dec.string Dec.string)))

        modelBuilder r c t =
            Model
                { refs = Dict.fromList r
                , commits = Dict.fromList c
                , treeObjects = Dict.fromList t
                , conflicts = [] -- TODO
                }
    in
    Dec.map3 modelBuilder
        (Dec.field "ref" (Dec.list refObjectDecoder))
        (Dec.field "commit" (Dec.list commitObjectDecoder))
        (Dec.field "tree" (Dec.list treeObjectDecoder))


encode : Model -> Enc.Value
encode (Model { commits, refs, treeObjects }) =
    let
        treeObjectToValue ( sha, treeObject ) =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "type", Enc.string "tree" )
                , ( "content", Enc.string treeObject.content )
                , ( "children"
                  , Enc.list (Enc.list Enc.string) (List.map (\( childSha, childId ) -> [ childSha, childId ]) treeObject.children)
                  )
                ]

        refToValue ( sha, ref ) =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "_rev", Enc.string ref.rev )
                , ( "type", Enc.string "ref" )
                , ( "value", Enc.string ref.value )
                , ( "ancestors", Enc.list Enc.string ref.ancestors )
                ]

        commitToValue ( sha, commitObj ) =
            Enc.object
                [ ( "_id", Enc.string sha )
                , ( "type", Enc.string "commit" )
                , ( "tree", Enc.string commitObj.tree )
                , ( "parents", Enc.list Enc.string commitObj.parents )
                , ( "author", Enc.string commitObj.author )
                , ( "timestamp", Enc.int commitObj.timestamp )
                ]
    in
    Enc.object
        [ ( "refs", Enc.list refToValue (Dict.toList refs) )
        , ( "commits", Enc.list commitToValue (Dict.toList commits) )
        , ( "treeObjects", Enc.list treeObjectToValue (Dict.toList treeObjects) )
        ]



-- HELPERS


conflict : Op -> Op -> Selection -> Conflict
conflict opA opB sel =
    Conflict "" opA opB sel False
        |> conflictWithSha
