module Doc.Data exposing (DataCmd(..), Model, Objects, checkout, commitNew, conflictList, conflictSelection, defaultObjects, empty, encode, received, resolve, success)

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
    = IsOk Data
    | InConflict Data ConflictInfo


type alias Data =
    { refs : Dict String RefObject
    , commits : Dict String CommitObject
    , treeObjects : Dict String TreeObject
    }


type alias ConflictInfo =
    { localHead : String
    , remoteHead : String
    , conflicts : List Conflict
    , mergedTree : Tree
    }


empty : Model
empty =
    IsOk
        { refs = Dict.empty
        , commits = Dict.empty
        , treeObjects = Dict.empty
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


received : Dec.Value -> ( Model, Tree ) -> ( Model, Tree, List Conflict )
received json ( oldModel, oldTree ) =
    case Dec.decodeValue decode json of
        Ok ( newData, Nothing ) ->
            ( IsOk newData, checkoutRef "heads/master" newData |> Maybe.withDefault oldTree, [] )

        Ok ( newData, Just ( confId, confHead ) ) ->
            let
                localHead =
                    Dict.get "heads/master" newData.refs |> Maybe.withDefault confHead

                mergedModel =
                    merge localHead.value confHead.value oldTree newData
            in
            case mergedModel of
                IsOk data ->
                    ( IsOk data, checkoutRef "heads/master" data |> Maybe.withDefault oldTree, [] )

                InConflict data cdata ->
                    ( InConflict data cdata, cdata.mergedTree, cdata.conflicts )

        Err err ->
            let
                _ =
                    Debug.log "error" err
            in
            ( oldModel, oldTree, [] )


checkoutRef : String -> Data -> Maybe Tree
checkoutRef refId data =
    Dict.get refId data.refs
        |> andThen (\ro -> Dict.get ro.value data.commits)
        |> andThen (\co -> treeObjectsToTree data.treeObjects co.tree "0")


success : Dec.Value -> Model -> Model
success json model =
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
                updater d ( id, newRev ) =
                    Dict.get id d.refs
                        |> Maybe.andThen (\r -> Just ( id, { r | rev = newRev } ))

                resDict d =
                    responses
                        |> List.filterMap (updater d)
                        |> Dict.fromList

                newRefs d =
                    Dict.union (resDict d) d.refs
            in
            case model of
                IsOk d ->
                    IsOk { d | refs = newRefs d }

                InConflict d cd ->
                    InConflict { d | refs = newRefs d } cd

        Err err ->
            model


commitNew : String -> Int -> Tree -> Model -> Model
commitNew author timestamp tree model =
    case model of
        IsOk data ->
            case Dict.get "heads/master" data.refs of
                Nothing ->
                    IsOk (commitTree author [] timestamp tree data)

                Just localHead ->
                    IsOk (commitTree author [ localHead.value ] timestamp tree data)

        InConflict data { localHead, remoteHead, conflicts } ->
            if List.isEmpty (List.filter (not << .resolved) conflicts) then
                -- No unresolved conflicts.
                IsOk (commitTree author [ localHead, remoteHead ] timestamp tree data)

            else
                -- Unresolved conflicts exist, dont' commit.
                model


checkout : String -> Objects -> ( Status, Maybe Tree )
checkout commitSha objects =
    ( Clean commitSha, checkoutCommit commitSha objects )


conflictList : Model -> List Conflict
conflictList model =
    case model of
        IsOk _ ->
            []

        InConflict _ { conflicts } ->
            conflicts


conflictSelection : String -> Selection -> Model -> Model
conflictSelection cid selection model =
    case model of
        InConflict data confInfo ->
            let
                newConflicts =
                    confInfo.conflicts
                        |> List.map
                            (\c ->
                                if c.id == cid then
                                    { c | selection = selection }

                                else
                                    c
                            )
            in
            InConflict data { confInfo | conflicts = newConflicts }

        IsOk _ ->
            model


resolve : String -> Model -> ( Model, Bool )
resolve cid model =
    case model of
        IsOk _ ->
            ( model, True )

        InConflict d confInfo ->
            let
                newConflicts =
                    List.filter (\c -> c.id /= cid) confInfo.conflicts
            in
            if List.isEmpty newConflicts then
                ( IsOk d, True )

            else
                ( InConflict d { confInfo | conflicts = newConflicts }, False )



-- INTERNALS


commitTree : String -> List String -> Int -> Tree -> Data -> Data
commitTree author parents timestamp tree data =
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
    { data
        | refs = Dict.update "heads/master" updateHead data.refs
        , commits = Dict.insert newCommitSha newCommit data.commits
        , treeObjects = Dict.union newTreeObjects data.treeObjects
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


merge : String -> String -> Tree -> Data -> Model
merge aSha bSha oldTree data =
    if aSha == bSha then
        IsOk data

    else if List.member bSha (getAncestors data.commits aSha) then
        IsOk data

    else if List.member aSha (getAncestors data.commits bSha) then
        IsOk data

    else
        let
            oSha =
                getCommonAncestor_ data.commits aSha bSha |> Maybe.withDefault ""

            getTree_ sha =
                Dict.get sha data.commits
                    |> Maybe.andThen (\co -> treeObjectsToTree data.treeObjects co.tree "0")

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
                InConflict data { localHead = aSha, remoteHead = bSha, conflicts = conflicts, mergedTree = mTree }

            ( Nothing, Just _, Just _ ) ->
                Debug.todo "failed merge, no common ancestor found."

            _ ->
                Debug.todo "failed merge"


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


decode : Dec.Decoder ( Data, Maybe ( String, RefObject ) )
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

        modelBuilder r c t cflct =
            ( Data (Dict.fromList r) (Dict.fromList c) (Dict.fromList t), cflct )
    in
    Dec.map4 modelBuilder
        (Dec.field "ref" (Dec.list refObjectDecoder))
        (Dec.field "commit" (Dec.list commitObjectDecoder))
        (Dec.field "tree" (Dec.list treeObjectDecoder))
        (Dec.maybe (Dec.field "conflict" refObjectDecoder))


encode : Model -> Enc.Value
encode model =
    let
        data =
            case model of
                IsOk d ->
                    d

                InConflict d _ ->
                    d

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
        [ ( "refs", Enc.list refToValue (Dict.toList data.refs) )
        , ( "commits", Enc.list commitToValue (Dict.toList data.commits) )
        , ( "treeObjects", Enc.list treeObjectToValue (Dict.toList data.treeObjects) )
        ]



-- HELPERS


conflict : Op -> Op -> Selection -> Conflict
conflict opA opB sel =
    Conflict "" opA opB sel False
        |> conflictWithSha
