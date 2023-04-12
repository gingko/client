module Doc.Data exposing (CommitObject, Model, cardDataReceived, conflictList, conflictToTree, convert, empty, getCommit, getHistoryList, gitDataReceived, hasConflicts, head, historyReceived, isGitLike, lastSavedTime, lastSyncedTime, localSave, pushOkHandler, requestCommit, resolve, resolveConflicts, restore, success, triggeredPush)

import Coders exposing (treeToValue, tupleDecoder)
import Dict exposing (Dict)
import Diff3 exposing (diff3Merge)
import Doc.Data.Conflict as Conf exposing (Conflict, Op(..), Selection(..), conflictWithSha, opString)
import Doc.TreeStructure exposing (apply, opToMsg)
import Http exposing (Error(..))
import Json.Decode as Dec
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe exposing (andThen)
import Outgoing exposing (Msg(..))
import RemoteData exposing (WebData)
import Set exposing (Set)
import Time
import Types exposing (CardTreeOp(..), Children(..), ConflictSelection(..), Tree)
import Utils exposing (hash)



-- MODEL


historyLimit =
    1


type Model
    = CardBased CardData (List ( String, Time.Posix, WebData CardData )) (Maybe CardDataConflicts)
    | GitLike GitData (Maybe ConflictInfo)


type alias Card t =
    { id : String
    , treeId : String
    , content : String
    , parentId : Maybe String
    , position : Float
    , deleted : Bool
    , synced : Bool
    , updatedAt : t
    }


type alias CardData =
    List (Card String)


type alias CardDataConflicts =
    { ours : CardData
    , theirs : CardData
    , original : CardData
    }


type alias GitData =
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


empty : Model
empty =
    GitLike emptyData Nothing


emptyData : GitData
emptyData =
    { refs = Dict.empty
    , commits = Dict.empty
    , treeObjects = Dict.empty
    }



-- EXPOSED : Getters


head : String -> Model -> Maybe String
head id model =
    case model of
        CardBased _ _ _ ->
            Nothing

        GitLike data _ ->
            Dict.get id data.refs |> Maybe.map .value


getCommit : String -> Model -> Maybe CommitObject
getCommit sha model =
    case model of
        CardBased _ _ _ ->
            Nothing

        GitLike data _ ->
            data
                |> .commits
                |> Dict.get sha


hasConflicts : Model -> Bool
hasConflicts model =
    case model of
        CardBased _ _ (Just _) ->
            True

        GitLike _ (Just _) ->
            True

        _ ->
            False


conflictList : Model -> List Conflict
conflictList model =
    case model of
        GitLike _ Nothing ->
            []

        GitLike _ (Just { conflicts }) ->
            conflicts

        CardBased _ _ _ ->
            []


restore : Model -> String -> List Outgoing.Msg
restore model historyId =
    case model of
        CardBased oldData history _ ->
            let
                newData_ =
                    history
                        |> List.filter (\( id, _, _ ) -> id == historyId)
                        |> List.head
                        |> Maybe.map (\( _, _, wd ) -> wd)
                        |> Maybe.andThen RemoteData.toMaybe
            in
            case newData_ of
                Just newData ->
                    let
                        oldDataDict =
                            oldData
                                |> List.map (\c -> ( c.updatedAt, c ))
                                |> Dict.fromList

                        newDataDict =
                            newData
                                |> List.map (\c -> ( c.updatedAt, c ))
                                |> Dict.fromList

                        toAdd =
                            Dict.diff newDataDict oldDataDict
                                |> Dict.toList
                                |> List.map Tuple.second
                                |> List.map stripUpdatedAt
                                |> List.map (\c -> { c | synced = False })
                    in
                    [ SaveCardBased (toSave { toAdd = toAdd, toMarkSynced = [], toMarkDeleted = [], toRemove = Set.empty }) ]

                _ ->
                    []

        GitLike data _ ->
            []


lastSavedTime : Model -> Maybe Int
lastSavedTime model =
    case model of
        CardBased data _ _ ->
            data
                |> List.map .updatedAt
                |> List.sort
                |> List.reverse
                |> List.head
                |> Maybe.andThen parseUpdatedAt

        GitLike data _ ->
            Nothing


lastSyncedTime : Model -> Maybe Int
lastSyncedTime model =
    case model of
        CardBased data _ _ ->
            data
                |> List.filter .synced
                |> List.map .updatedAt
                |> List.sort
                |> List.reverse
                |> List.head
                |> Maybe.andThen parseUpdatedAt

        GitLike data _ ->
            data.commits
                |> Dict.values
                |> List.map .timestamp
                |> List.sort
                |> List.reverse
                |> List.head


parseUpdatedAt : String -> Maybe Int
parseUpdatedAt str =
    String.split ":" str
        |> List.head
        |> Maybe.andThen String.toInt


isGitLike : Model -> Bool
isGitLike model =
    case model of
        GitLike _ _ ->
            True

        CardBased _ _ _ ->
            False



-- EXPOSED : Functions


cardDataReceived : Dec.Value -> ( Model, Tree, String ) -> Maybe { newData : Model, newTree : Tree, outMsg : List Outgoing.Msg }
cardDataReceived json ( oldModel, oldTree, treeId ) =
    case Dec.decodeValue decodeCards json of
        Ok cards ->
            let
                newModelWithoutConflicts =
                    case oldModel of
                        CardBased oldData oldHistory oldConflicts_ ->
                            if cards /= oldData then
                                let
                                    latestUpdatedAt =
                                        cards
                                            |> List.map .updatedAt
                                            |> List.sort
                                            |> List.reverse
                                            |> List.head
                                            |> Maybe.withDefault ""

                                    latestTs =
                                        latestUpdatedAt
                                            |> String.split ":"
                                            |> List.head
                                            |> Maybe.andThen String.toInt
                                            |> Maybe.withDefault 0
                                            |> Time.millisToPosix
                                in
                                CardBased cards (( latestUpdatedAt, latestTs, RemoteData.Success cards ) :: oldHistory |> ListExtra.uniqueBy (\( _, ts, _ ) -> Time.posixToMillis ts)) oldConflicts_

                            else
                                oldModel

                        GitLike _ _ ->
                            CardBased cards [] Nothing

                newTree =
                    cards
                        |> toTree

                syncState =
                    getSyncState cards

                ( outMsg, conflicts_ ) =
                    case syncState of
                        Unsynced ->
                            ( [ PushDeltas (pushDelta treeId cards) ]
                            , Nothing
                            )

                        CanFastForward ffids ->
                            ( [ SaveCardBased (toSave { toAdd = [], toMarkSynced = [], toMarkDeleted = [], toRemove = ffids |> Set.fromList }) ]
                            , Nothing
                            )

                        Conflicted conflictData ->
                            let
                                mergedChanges =
                                    resolveDeleteConflicts cards conflictData
                            in
                            if List.length mergedChanges.toAdd > 0 || List.length mergedChanges.toMarkSynced > 0 || Set.size mergedChanges.toRemove > 0 then
                                ( [ SaveCardBased (toSave mergedChanges) ]
                                , Nothing
                                )

                            else
                                ( [], Just { ours = conflictData.ours, theirs = conflictData.theirs, original = conflictData.original } )

                        _ ->
                            ( [], Nothing )

                newModel =
                    case newModelWithoutConflicts of
                        CardBased data history _ ->
                            CardBased data history conflicts_

                        GitLike _ _ ->
                            newModelWithoutConflicts
            in
            if (newModel /= oldModel) || (newTree /= oldTree) then
                Just { newData = newModel, newTree = newTree, outMsg = outMsg }

            else
                Nothing

        Err err ->
            Nothing


triggeredPush : Model -> String -> List Outgoing.Msg
triggeredPush model treeId =
    case model of
        CardBased cards _ _ ->
            let
                syncState =
                    getSyncState cards
            in
            case syncState of
                Unsynced ->
                    [ PushDeltas (pushDelta treeId cards) ]

                _ ->
                    []

        GitLike _ _ ->
            []


resolveConflicts : ConflictSelection -> Model -> Maybe Outgoing.Msg
resolveConflicts selectedVersion model =
    case model of
        CardBased _ _ (Just versions) ->
            let
                ( toAdd, toRemove ) =
                    case selectedVersion of
                        Types.Original ->
                            ( versions.original |> List.map (\c -> { c | synced = False } |> stripUpdatedAt)
                            , versions.original |> List.map .updatedAt |> Set.fromList
                            )

                        Types.Theirs ->
                            ( []
                            , (versions.original ++ versions.ours)
                                |> List.map .updatedAt
                                |> Set.fromList
                            )

                        Types.Ours ->
                            ( []
                            , versions.original
                                |> List.map .updatedAt
                                |> Set.fromList
                            )
            in
            SaveCardBased (toSave { toAdd = toAdd, toMarkSynced = [], toMarkDeleted = [], toRemove = toRemove }) |> Just

        _ ->
            Nothing


gitDataReceived : Dec.Value -> ( Model, Tree ) -> Maybe { newData : Model, newTree : Tree }
gitDataReceived json ( oldModel, oldTree ) =
    case Dec.decodeValue decodeGitLike json of
        Ok ( newData, Nothing ) ->
            { newData = GitLike newData Nothing
            , newTree = checkoutRef "heads/master" newData |> Maybe.withDefault oldTree
            }
                |> Just

        Ok ( newData, Just ( _, confHead ) ) ->
            let
                localHead =
                    Dict.get "heads/master" newData.refs |> Maybe.withDefault confHead

                mergedModel =
                    merge localHead.value confHead.value oldTree newData
            in
            case mergedModel of
                ( data, Nothing ) ->
                    { newData = GitLike data Nothing
                    , newTree = checkoutRef "heads/master" data |> Maybe.withDefault oldTree
                    }
                        |> Just

                ( data, Just cdata ) ->
                    { newData = GitLike data (Just cdata)
                    , newTree = cdata.mergedTree
                    }
                        |> Just

        Err err ->
            Nothing


success : Dec.Value -> Model -> Model
success json model =
    case Dec.decodeValue decodeGitLike json of
        Ok ( newData, conflict_ ) ->
            let
                updateData d =
                    { d
                        | refs = Dict.union newData.refs d.refs
                        , commits = Dict.union newData.commits d.commits
                        , treeObjects = Dict.union newData.treeObjects d.treeObjects
                    }
            in
            case model of
                CardBased _ _ _ ->
                    model

                GitLike d cd_ ->
                    GitLike (updateData d) cd_

        Err err ->
            model


conflictToTree : Model -> ConflictSelection -> Maybe Tree
conflictToTree data selection =
    case data of
        CardBased _ _ (Just cd) ->
            case selection of
                Types.Ours ->
                    cd.ours |> toTree |> Just

                Types.Theirs ->
                    cd.theirs |> toTree |> Just

                Types.Original ->
                    cd.original |> toTree |> Just

        _ ->
            Nothing


resolve : String -> Model -> Model
resolve cid model =
    case model of
        CardBased _ _ _ ->
            model

        GitLike _ Nothing ->
            model

        GitLike d (Just confInfo) ->
            let
                newConflicts =
                    List.filter (\c -> c.id /= cid) confInfo.conflicts
            in
            GitLike d (Just { confInfo | conflicts = newConflicts })


convert : String -> Model -> Maybe ( Model, Enc.Value )
convert docId model =
    case model of
        GitLike _ _ ->
            let
                gitLikeHistory =
                    getHistoryList model

                latestVersion =
                    gitLikeHistory |> List.reverse |> List.head

                cardHistory =
                    gitLikeHistory
                        |> List.map
                            (\( i, t, tr_ ) ->
                                ( i
                                , t
                                , RemoteData.fromMaybe (BadBody "Couldn't import git-like history") tr_
                                    |> RemoteData.map (fromTree docId 0 Nothing t 0)
                                )
                            )
            in
            case latestVersion of
                Just ( _, ts, Just tree ) ->
                    let
                        currCards =
                            fromTree docId 0 Nothing ts 0 tree
                    in
                    Just
                        ( CardBased currCards cardHistory Nothing
                        , Enc.list encodeExistingCard currCards
                        )

                _ ->
                    Nothing

        CardBased data _ _ ->
            Nothing



-- INTERNALS


checkoutRef : String -> GitData -> Maybe Tree
checkoutRef refId data =
    Dict.get refId data.refs
        |> andThen (\ro -> Dict.get ro.value data.commits)
        |> andThen (\co -> treeObjectsToTree data.treeObjects co.tree "0")


checkoutCommit : String -> GitData -> Maybe Tree
checkoutCommit commitSha data =
    Dict.get commitSha data.commits
        |> andThen (\co -> treeObjectsToTree data.treeObjects co.tree "0")


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



-- ==== Merging


merge : String -> String -> Tree -> GitData -> ( GitData, Maybe ConflictInfo )
merge aSha bSha _ data =
    if aSha == bSha then
        ( data, Nothing )

    else if List.member bSha (getAncestors data.commits aSha) then
        ( data, Nothing )

    else if List.member aSha (getAncestors data.commits bSha) then
        ( data, Nothing )

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
                if List.isEmpty conflicts then
                    ( data, Nothing )

                else
                    ( data, Just { localHead = aSha, remoteHead = bSha, conflicts = conflicts, mergedTree = mTree } )

            ( Nothing, Just _, Just _ ) ->
                ( data, Nothing )

            _ ->
                ( data, Nothing )


mergeTreeStructure : Tree -> Tree -> Tree -> ( Tree, List Conflict )
mergeTreeStructure oTree aTree bTree =
    let
        ( cleanOps, conflicts ) =
            getConflicts (getOps oTree aTree) (getOps oTree bTree)
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
            ops ++ modOp ++ movOp

        ignoreOp : Tree -> Op -> Op -> Bool
        ignoreOp _ op1 op2 =
            case ( op1, op2 ) of
                ( Del _ parents1, Del id2 _ ) ->
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
        conflict opA opB sel =
            Conflict "" opA opB sel False
                |> conflictWithSha

        liftFn : Op -> Op -> ( List Op, List Conflict )
        liftFn opA opB =
            case ( opA, opB ) of
                -- Modify/Modify conflict
                ( Mod idA pidsA strA orig, Mod idB _ strB _ ) ->
                    if idA == idB && strA /= strB then
                        case diff3Merge (String.lines strA) (String.lines orig) (String.lines strB) of
                            [ Diff3.DiffOk mergedStrings ] ->
                                ( [ Mod idA pidsA (mergedStrings |> String.join "\n") orig ], [] )

                            _ ->
                                ( [], [ conflict opA opB Manual ] )

                    else
                        ( [ opA, opB ], [] )

                -- Modify/Delete conflicts
                ( Mod idA pidsA _ _, Del idB _ ) ->
                    if idA == idB || List.member idB pidsA then
                        ( [], [ conflict opA opB Conf.Ours ] )

                    else
                        ( [ opA, opB ], [] )

                ( Del idA _, Mod idB pidsB _ _ ) ->
                    if idA == idB || List.member idA pidsB then
                        ( [], [ conflict opA opB Conf.Theirs ] )

                    else
                        ( [ opA, opB ], [] )

                -- Insert/Delete conflicts
                ( Ins idA _ pidsA _, Del idB _ ) ->
                    if idA == idB || List.member idB pidsA then
                        ( [], [ conflict opA opB Conf.Ours ] )

                    else
                        ( [ opA, opB ], [] )

                ( Del idA _, Ins idB _ pidsB _ ) ->
                    if idA == idB || List.member idA pidsB then
                        ( [], [ conflict opA opB Conf.Theirs ] )

                    else
                        ( [ opA, opB ], [] )

                ( Mov idA _ _ newParentsA _, Mov idB _ _ newParentsB _ ) ->
                    if areAcyclicMoves ( idA, newParentsA ) ( idB, newParentsB ) then
                        ( [], [ conflict opA opB Conf.Ours ] )

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
        |> (\( os, cs ) -> ( os |> ListExtra.uniqueBy opString, cs |> ListExtra.uniqueBy .id ))



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


type DataIn
    = GitLikeIn ( GitData, Maybe ( String, RefObject ) )
    | CardBasedIn (List (Card String))


decodeCards : Dec.Decoder (List (Card String))
decodeCards =
    Dec.list decodeCard


decodeCard : Dec.Decoder (Card String)
decodeCard =
    Dec.map8 Card
        (Dec.field "id" Dec.string)
        (Dec.field "treeId" Dec.string)
        (Dec.field "content" Dec.string)
        (Dec.field "parentId" (Dec.maybe Dec.string))
        (Dec.field "position" Dec.float)
        (Dec.field "deleted" intToBool)
        (Dec.field "synced" Dec.bool)
        (Dec.field "updatedAt" Dec.string)


toSave : DBChangeLists -> Enc.Value
toSave { toAdd, toMarkSynced, toMarkDeleted, toRemove } =
    Enc.object
        [ ( "toAdd", Enc.list encodeNewCard toAdd )
        , ( "toMarkSynced", Enc.list encodeExistingCard toMarkSynced )
        , ( "toMarkDeleted", Enc.list encodeNewCard toMarkDeleted )
        , ( "toRemove", Enc.list Enc.string (Set.toList toRemove) )
        ]


stripUpdatedAt : Card String -> Card ()
stripUpdatedAt card =
    { id = card.id
    , treeId = card.treeId
    , content = card.content
    , parentId = card.parentId
    , position = card.position
    , deleted = card.deleted
    , synced = card.synced
    , updatedAt = ()
    }


encodeNewCard : Card () -> Enc.Value
encodeNewCard card =
    Enc.object
        [ ( "id", Enc.string card.id )
        , ( "treeId", Enc.string card.treeId )
        , ( "content", Enc.string card.content )
        , ( "parentId", encodeMaybe card.parentId )
        , ( "position", Enc.float card.position )
        , ( "deleted", Enc.int (boolToInt card.deleted) )
        , ( "synced", Enc.bool card.synced )
        , ( "updatedAt", Enc.string "" )
        ]


encodeExistingCard : Card String -> Enc.Value
encodeExistingCard card =
    Enc.object
        [ ( "id", Enc.string card.id )
        , ( "treeId", Enc.string card.treeId )
        , ( "content", Enc.string card.content )
        , ( "parentId", encodeMaybe card.parentId )
        , ( "position", Enc.float card.position )
        , ( "deleted", Enc.int (boolToInt card.deleted) )
        , ( "synced", Enc.bool card.synced )
        , ( "updatedAt", Enc.string card.updatedAt )
        ]


decodeGitLike : Dec.Decoder ( GitData, Maybe ( String, RefObject ) )
decodeGitLike =
    let
        modelBuilder r c t cflct =
            ( GitData (Dict.fromList r) (Dict.fromList c) (Dict.fromList t), cflct )
    in
    Dec.map4 modelBuilder
        (Dec.field "ref" (Dec.list refObjectDecoder))
        (Dec.field "commit" (Dec.list commitObjectDecoder))
        (Dec.field "tree" (Dec.list treeObjectDecoder))
        (Dec.maybe (Dec.field "conflict" refObjectDecoder))


refObjectDecoder : Dec.Decoder ( String, RefObject )
refObjectDecoder =
    Dec.map4 (\id v a r -> ( id, RefObject v a r ))
        (Dec.field "_id" Dec.string)
        (Dec.field "value" Dec.string)
        (Dec.field "ancestors" (Dec.list Dec.string))
        (Dec.field "_rev" Dec.string)


commitObjectDecoder : Dec.Decoder ( String, CommitObject )
commitObjectDecoder =
    Dec.map5 (\id t p a ts -> ( id, CommitObject t p a ts ))
        (Dec.field "_id" Dec.string)
        (Dec.field "tree" Dec.string)
        (Dec.field "parents" (Dec.list Dec.string))
        (Dec.field "author" Dec.string)
        (Dec.field "timestamp" Dec.int)


treeObjectDecoder : Dec.Decoder ( String, TreeObject )
treeObjectDecoder =
    Dec.map3 (\id cn ch -> ( id, TreeObject cn ch ))
        (Dec.field "_id" Dec.string)
        (Dec.field "content" Dec.string)
        (Dec.field "children" (Dec.list (tupleDecoder Dec.string Dec.string)))


requestCommit : Tree -> String -> Model -> Enc.Value -> Maybe Enc.Value
requestCommit workingTree author model metadata =
    case model of
        CardBased _ _ _ ->
            Nothing

        GitLike data Nothing ->
            case Dict.get "heads/master" data.refs of
                Nothing ->
                    Enc.object
                        [ ( "workingTree", treeToValue workingTree )
                        , ( "author", Enc.string author )
                        , ( "parents", Enc.list Enc.string [] )
                        , ( "metadata", metadata )
                        ]
                        |> Just

                Just localHead ->
                    Enc.object
                        [ ( "workingTree", treeToValue workingTree )
                        , ( "author", Enc.string author )
                        , ( "parents", Enc.list Enc.string [ localHead.value ] )
                        , ( "metadata", metadata )
                        ]
                        |> Just

        GitLike data (Just { localHead, remoteHead, conflicts }) ->
            if List.isEmpty (List.filter (not << .resolved) conflicts) then
                -- No unresolved conflicts.
                Enc.object
                    [ ( "workingTree", treeToValue workingTree )
                    , ( "author", Enc.string author )
                    , ( "parents", Enc.list Enc.string [ localHead, remoteHead ] )
                    , ( "metadata", metadata )
                    ]
                    |> Just

            else
                -- Unresolved conflicts exist, dont' commit.
                Nothing



---


type alias DBChangeLists =
    { toAdd : List (Card ())
    , toMarkSynced : List (Card String)
    , toMarkDeleted : List (Card ())
    , toRemove : Set String
    }


localSave : String -> CardTreeOp -> Model -> Enc.Value
localSave treeId op model =
    case model of
        CardBased data _ _ ->
            case op of
                CTUpd id newContent ->
                    let
                        toAdd =
                            data
                                |> List.filter (\card -> card.id == id)
                                |> List.head
                                |> Maybe.map (\card -> [ { card | content = newContent, synced = False } |> stripUpdatedAt ])
                                |> Maybe.withDefault []
                    in
                    toSave { toAdd = toAdd, toMarkSynced = [], toMarkDeleted = [], toRemove = Set.empty }

                CTIns id content parId_ idx ->
                    let
                        toAdd =
                            [ { id = id, treeId = treeId, content = content, parentId = parId_, position = getPosition id parId_ idx data, deleted = False, synced = False, updatedAt = () } ]
                    in
                    toSave { toAdd = toAdd, toMarkSynced = [], toMarkDeleted = [], toRemove = Set.empty }

                CTRmv id ->
                    let
                        idsToMarkAsDeleted =
                            getDescendants id data

                        cardsToMarkAsDeleted =
                            data
                                |> List.filter (\card -> List.member card.id idsToMarkAsDeleted)
                                |> List.sortBy .updatedAt
                                |> ListExtra.uniqueBy .id
                                |> List.map (\card -> { card | deleted = True, synced = False } |> stripUpdatedAt)
                    in
                    toSave { toAdd = [], toMarkSynced = [], toMarkDeleted = cardsToMarkAsDeleted, toRemove = Set.empty }

                CTMov id parId_ idx ->
                    let
                        toAdd =
                            data
                                |> List.filter (\card -> card.id == id)
                                |> List.head
                                |> Maybe.map (\card -> [ { card | position = getPosition id parId_ idx data, parentId = parId_, synced = False } |> stripUpdatedAt ])
                                |> Maybe.withDefault []
                    in
                    toSave { toAdd = toAdd, toMarkSynced = [], toMarkDeleted = [], toRemove = Set.empty }

                CTMrg aId bId bool ->
                    Enc.null

                CTBlk tree string int ->
                    Enc.null

        GitLike gitData maybeConflictInfo ->
            Enc.null


getPosition : String -> Maybe String -> Int -> List (Card String) -> Float
getPosition cardId parId idx data =
    let
        siblings =
            data
                |> List.filter (\card -> card.parentId == parId && card.deleted == False && card.id /= cardId)
                |> List.sortBy .updatedAt
                |> ListExtra.uniqueBy .id
                |> List.sortBy .position

        ( sibLeft_, sibRight_ ) =
            case idx of
                999999 ->
                    ( ListExtra.last siblings |> Maybe.map .position, Nothing )

                _ ->
                    ( ListExtra.getAt (idx - 1) siblings |> Maybe.map .position
                    , ListExtra.getAt idx siblings |> Maybe.map .position
                    )
    in
    case ( sibLeft_, sibRight_ ) of
        ( Just sibLeft, Just sibRight ) ->
            (sibLeft + sibRight)
                / 2

        ( Just sibLeft, Nothing ) ->
            sibLeft
                + 1

        ( Nothing, Just sibRight ) ->
            sibRight
                - 1

        ( Nothing, Nothing ) ->
            0


fromTree : String -> Int -> Maybe String -> Time.Posix -> Int -> Tree -> List (Card String)
fromTree treeId depth parId ts idx tree =
    if tree.id == "0" then
        case tree.children of
            Children children ->
                children
                    |> List.indexedMap (fromTree treeId depth Nothing ts)
                    |> List.concat

    else
        let
            tsInt =
                Time.posixToMillis ts
        in
        { id = treeId ++ ":" ++ tree.id, treeId = treeId, content = tree.content, parentId = parId |> Maybe.map (\pid -> treeId ++ ":" ++ pid), position = toFloat idx, deleted = False, synced = False, updatedAt = (tsInt |> String.fromInt) ++ ":" ++ String.fromInt depth ++ ":" ++ hash tsInt tree.id }
            :: (case tree.children of
                    Children children ->
                        children
                            |> List.indexedMap (fromTree treeId (depth + 1) (Just tree.id) ts)
                            |> List.concat
               )


toTree : List (Card String) -> Tree
toTree allCards =
    Tree "0" "" (Children (toTrees allCards))


toTrees : List (Card String) -> List Tree
toTrees allCards =
    let
        cards =
            allCards
                |> List.sortBy .updatedAt
                |> List.reverse
                |> ListExtra.uniqueBy .id
                |> List.filter (not << .deleted)
    in
    treeHelper cards Nothing


treeHelper : List (Card String) -> Maybe String -> List Tree
treeHelper allCards parentId =
    let
        cards =
            allCards |> List.filter (\card -> card.parentId == parentId) |> List.sortBy .position
    in
    List.map (\card -> { id = card.id, content = card.content, children = Children (treeHelper allCards (Just card.id)) }) cards


getDescendants : String -> List (Card String) -> List String
getDescendants id allCards =
    let
        card_ =
            allCards |> List.filter (\card -> card.id == id) |> List.head
    in
    case card_ of
        Nothing ->
            []

        Just card ->
            card.id
                :: (allCards
                        |> List.filter (\c -> c.parentId == Just id)
                        |> List.map .id
                        |> List.concatMap (\i -> getDescendants i allCards)
                   )


type alias Versions =
    { original : List (Card String)
    , ours : List (Card String)
    , theirs : List (Card String)
    }


type SyncState
    = Synced
    | Unsynced
    | CanFastForward (List String)
    | Conflicted Versions
    | Errored


getCardById : List (Card String) -> String -> Maybe (Card String)
getCardById db id =
    db
        |> List.filter (\card -> card.id == id)
        |> List.sortBy .updatedAt
        |> List.reverse
        |> List.head


getSyncState : List (Card String) -> SyncState
getSyncState db =
    let
        cardSyncStates =
            db
                |> ListExtra.gatherWith (\a b -> a.id == b.id)
                |> List.map (\( a, rest ) -> a :: rest)
                |> List.map
                    (\c ->
                        let
                            cardSyncState =
                                getCardSyncState c

                            cardId =
                                c
                                    |> List.head
                                    |> Maybe.map .id
                                    |> Maybe.withDefault ""
                        in
                        ( cardId, cardSyncState )
                    )
    in
    if
        List.any
            (\( _, s ) ->
                case s of
                    Conflicted _ ->
                        True

                    _ ->
                        False
            )
            cardSyncStates
    then
        let
            versions =
                cardSyncStates
                    |> List.filterMap
                        (\( _, s ) ->
                            case s of
                                Conflicted v ->
                                    Just v

                                _ ->
                                    Nothing
                        )

            allOrig =
                versions |> List.concatMap .original

            allOurs =
                versions |> List.concatMap .ours

            allTheirs =
                versions |> List.concatMap .theirs
        in
        Conflicted { original = allOrig, ours = allOurs, theirs = allTheirs }

    else if List.any (\( _, s ) -> s == Unsynced) cardSyncStates then
        Unsynced

    else if List.all (\( _, s ) -> s == Synced) cardSyncStates then
        Synced

    else if
        List.any
            (\( _, s ) ->
                case s of
                    CanFastForward _ ->
                        True

                    _ ->
                        False
            )
            cardSyncStates
    then
        let
            allFFids =
                cardSyncStates
                    |> List.filterMap
                        (\( _, s ) ->
                            case s of
                                CanFastForward ids ->
                                    Just ids

                                _ ->
                                    Nothing
                        )
                    |> List.concat
        in
        CanFastForward allFFids

    else
        Errored


getCardSyncState : List (Card String) -> SyncState
getCardSyncState cardVersions =
    let
        ( syncedVersions, unsyncedVersions ) =
            cardVersions
                |> List.partition .synced
                |> Tuple.mapBoth List.length List.length

        versions =
            { original = getOriginals cardVersions
            , ours = getOurs cardVersions
            , theirs = getTheirs cardVersions
            }
    in
    if unsyncedVersions == 1 && syncedVersions == 0 && (List.map .content versions.ours == [ "" ]) then
        -- Brand new card with empty content shouldn't be pushed, so we mark it as "Synced" to prevent that.
        Synced

    else if unsyncedVersions > 0 && syncedVersions <= historyLimit then
        Unsynced

    else if unsyncedVersions > 0 && syncedVersions > historyLimit then
        Conflicted versions

    else if unsyncedVersions == 0 && syncedVersions > 0 && syncedVersions <= historyLimit then
        Synced

    else if unsyncedVersions == 0 && syncedVersions > historyLimit then
        let
            ids =
                cardVersions
                    |> List.map .updatedAt
                    |> List.sort
                    |> List.reverse
                    |> List.drop historyLimit
        in
        CanFastForward ids

    else
        Errored


getOriginals : List (Card String) -> List (Card String)
getOriginals db =
    db
        |> List.filter .synced
        |> List.sortBy .updatedAt
        |> List.head
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


getOurs : List (Card String) -> List (Card String)
getOurs db =
    db
        |> List.filter (not << .synced)
        |> List.sortBy .updatedAt
        |> List.reverse
        |> List.head
        |> Maybe.map List.singleton
        |> Maybe.withDefault []


getTheirs : List (Card String) -> List (Card String)
getTheirs db =
    db
        |> List.filter .synced
        |> List.sortBy .updatedAt
        |> List.reverse
        |> (\l ->
                if List.length l > historyLimit then
                    [ List.head l ]

                else
                    [ Nothing ]
           )
        |> List.filterMap identity


resolveDeleteConflicts : List (Card String) -> Versions -> DBChangeLists
resolveDeleteConflicts allCards versions =
    let
        idsOfConflicts =
            (versions.original ++ versions.ours ++ versions.theirs)
                |> List.map .id
                |> ListExtra.unique

        conflictPerCard : String -> { original : Maybe (Card String), ours : Maybe (Card String), theirs : Maybe (Card String) }
        conflictPerCard cardId =
            { original = ListExtra.find (\c -> c.id == cardId) versions.original
            , ours = ListExtra.find (\c -> c.id == cardId) versions.ours
            , theirs = ListExtra.find (\c -> c.id == cardId) versions.theirs
            }

        ( ourDeletionHashes, theirDeletionHashes ) =
            idsOfConflicts
                |> List.map conflictPerCard
                |> List.concatMap
                    (\v ->
                        case ( v.original, v.ours, v.theirs ) of
                            ( Just _, Just ours, Just thr ) ->
                                if ours.deleted && not thr.deleted then
                                    [ ( True, ours.updatedAt ) ]

                                else if not ours.deleted && thr.deleted then
                                    [ ( False, thr.updatedAt ) ]

                                else
                                    []

                            _ ->
                                []
                    )
                |> List.filterMap
                    (\( ot, ts ) ->
                        ts
                            |> String.split ":"
                            |> List.reverse
                            |> List.head
                            |> Maybe.map (\h -> ( ot, h ))
                    )
                |> List.partition Tuple.first
                |> Tuple.mapBoth (List.map Tuple.second) (List.map Tuple.second)

        ourDeletionTimestamps =
            -- If the delete conflict is because we deleted it on 'Our' side, then we need to undo those deletions
            -- by removing our unsynced deletions from the DB
            allCards
                |> List.filter (\c -> c.updatedAt |> String.split ":" |> List.reverse |> List.head |> Maybe.map (\h -> List.member h ourDeletionHashes) |> Maybe.withDefault False)
                |> List.map .updatedAt
                |> Set.fromList

        theirDeletionIds =
            allCards
                |> List.filter (\c -> c.updatedAt |> String.split ":" |> List.reverse |> List.head |> Maybe.map (\h -> List.member h theirDeletionHashes) |> Maybe.withDefault False)
                |> List.map .id
                |> ListExtra.unique

        theirDeletionCards =
            allCards
                |> List.filter
                    (\c ->
                        List.member c.id theirDeletionIds
                            && c.synced
                            && not (theirDeletionHashes |> List.member (c.updatedAt |> String.split ":" |> List.reverse |> List.head |> Maybe.withDefault ""))
                    )

        theirDeletionsToRemove =
            -- If the delete conflict is because they deleted it on 'Their' side, then we need to undo those deletions
            -- by removing the older synced version from the DB...
            theirDeletionCards
                |> List.map .updatedAt
                |> Set.fromList

        theirDeletionsLocalVersions =
            -- ... and add new unsynced undeleted versions so we can create deltas based off them
            theirDeletionCards
                |> List.filter (\c -> not (List.member c.id idsOfConflicts))
                |> List.map (\c -> { c | synced = False, deleted = False } |> stripUpdatedAt)
    in
    { toAdd = theirDeletionsLocalVersions, toMarkSynced = [], toMarkDeleted = [], toRemove = Set.union ourDeletionTimestamps theirDeletionsToRemove }


pushOkHandler : String -> Model -> Maybe Outgoing.Msg
pushOkHandler chk model =
    case model of
        CardBased data _ _ ->
            let
                cardsToSync =
                    data
                        |> List.filter (\card -> card.synced == False && card.updatedAt <= chk)
                        |> List.map (\card -> { card | synced = True })

                dbAfterSync =
                    cardsToSync
                        ++ data
                        |> ListExtra.uniqueBy .updatedAt

                syncedCardsOverLimit c =
                    c
                        |> List.map .updatedAt
                        |> List.sort
                        |> List.reverse
                        |> List.drop historyLimit

                versionsToDelete =
                    dbAfterSync
                        |> List.filter (\card -> card.synced == True)
                        |> ListExtra.gatherWith (\a b -> a.id == b.id)
                        |> List.map (\( a, rest ) -> a :: rest)
                        |> List.concatMap syncedCardsOverLimit
                        |> Set.fromList
            in
            Just <| SaveCardBased <| toSave { toAdd = [], toMarkSynced = cardsToSync, toMarkDeleted = [], toRemove = versionsToDelete }

        _ ->
            Nothing



-- Deltas


type alias Delta =
    { id : String, treeId : String, ts : String, ops : List CardOp }


type CardOp
    = InsOp { id : String, content : String, parentId : Maybe String, position : Float }
    | UpdOp { content : String, expectedVersion : String }
    | MovOp { parentId : Maybe String, position : Float }
    | DelOp { expectedVersion : String }
    | UndelOp


pushDelta : String -> List (Card String) -> Enc.Value
pushDelta treeId db =
    let
        deltas =
            toDelta treeId db

        checkpoint =
            db
                |> List.filter .synced
                |> List.map .updatedAt
                |> List.maximum
                |> Maybe.withDefault "0"
    in
    Enc.object
        [ ( "dlts", Enc.list encodeDelta deltas )
        , ( "tr", Enc.string treeId )
        , ( "chk", Enc.string checkpoint )
        ]


toDelta : String -> List (Card String) -> List Delta
toDelta treeId cards =
    cards
        |> List.map .id
        |> ListExtra.unique
        |> List.concatMap (cardDelta treeId cards)


cardDelta : String -> List (Card String) -> String -> List Delta
cardDelta treeId allCards cardId =
    let
        cardVersions =
            allCards
                |> List.filter (\c -> c.id == cardId)
                |> List.sortBy .updatedAt
                |> List.reverse

        unsyncedCard_ =
            cardVersions
                |> List.filter (not << .synced)
                |> List.head

        syncedCard_ =
            cardVersions
                |> List.filter .synced
                |> List.head
    in
    case ( unsyncedCard_, syncedCard_ ) of
        ( Just unsyncedCard, Just syncedCard ) ->
            let
                updateOps =
                    if unsyncedCard.content /= syncedCard.content then
                        [ UpdOp { content = unsyncedCard.content, expectedVersion = syncedCard.updatedAt } ]

                    else
                        []

                moveOps =
                    if (unsyncedCard.parentId /= syncedCard.parentId) || (unsyncedCard.position /= syncedCard.position) then
                        [ MovOp { parentId = unsyncedCard.parentId, position = unsyncedCard.position } ]

                    else
                        []

                ( deleteOps, undeleteOps ) =
                    if unsyncedCard.deleted && not syncedCard.deleted then
                        ( [ DelOp { expectedVersion = syncedCard.updatedAt } ], [] )

                    else if not unsyncedCard.deleted && syncedCard.deleted then
                        ( [], [ UndelOp ] )

                    else
                        ( [], [] )
            in
            [ Delta cardId treeId unsyncedCard.updatedAt (undeleteOps ++ deleteOps ++ moveOps ++ updateOps) ]

        ( Just unsyncedCard, Nothing ) ->
            [ Delta cardId treeId unsyncedCard.updatedAt [ InsOp { id = unsyncedCard.id, content = unsyncedCard.content, parentId = unsyncedCard.parentId, position = unsyncedCard.position } ] ]

        ( Nothing, Just _ ) ->
            -- Unchanged
            []

        ( Nothing, Nothing ) ->
            -- Error
            []


encodeDelta : Delta -> Enc.Value
encodeDelta delta =
    Enc.object
        [ ( "id", Enc.string delta.id )
        , ( "ts", Enc.string delta.ts )
        , ( "ops", Enc.list opEncoder delta.ops )
        ]


opEncoder : CardOp -> Enc.Value
opEncoder op =
    case op of
        InsOp insOp ->
            Enc.object
                [ ( "t", Enc.string "i" )
                , ( "c", Enc.string insOp.content )
                , ( "p", encodeMaybe insOp.parentId )
                , ( "pos", Enc.float insOp.position )
                ]

        UpdOp updOp ->
            Enc.object
                [ ( "t", Enc.string "u" )
                , ( "c", Enc.string updOp.content )
                , ( "e", Enc.string updOp.expectedVersion )
                ]

        MovOp movOp ->
            Enc.object
                [ ( "t", Enc.string "m" )
                , ( "p", encodeMaybe movOp.parentId )
                , ( "pos", Enc.float movOp.position )
                ]

        DelOp delOp ->
            Enc.object
                [ ( "t", Enc.string "d" )
                , ( "e", Enc.string delOp.expectedVersion )
                ]

        UndelOp ->
            Enc.object
                [ ( "t", Enc.string "ud" )
                ]



-- HISTORY


historyReceived : Dec.Value -> Model -> Model
historyReceived json model =
    case model of
        CardBased data oldHistory conflicts_ ->
            case Dec.decodeValue decodeHistory json of
                Ok history ->
                    let
                        oldHistoryDict : Dict String ( Time.Posix, WebData CardData )
                        oldHistoryDict =
                            oldHistory
                                |> List.map (\( id, ts, cardData ) -> ( id, ( ts, cardData ) ))
                                |> Dict.fromList

                        newHistoryDict : Dict String ( Time.Posix, WebData CardData )
                        newHistoryDict =
                            history
                                |> List.map (\( id, ts, cardData_ ) -> ( id, ( ts, RemoteData.fromMaybe (BadBody "Couldn't load history data") cardData_ ) ))
                                |> Dict.fromList

                        inBoth : String -> ( Time.Posix, WebData CardData ) -> ( Time.Posix, WebData CardData ) -> List ( String, Time.Posix, WebData CardData ) -> List ( String, Time.Posix, WebData CardData )
                        inBoth id ( tsL, cardDataL ) ( tsR, cardDataR ) acc =
                            case ( cardDataL, cardDataR ) of
                                ( RemoteData.Success _, _ ) ->
                                    ( id, tsL, cardDataL ) :: acc

                                ( _, RemoteData.Success _ ) ->
                                    ( id, tsR, cardDataR ) :: acc

                                _ ->
                                    ( id, tsL, cardDataL ) :: acc

                        newHistory : List ( String, Time.Posix, WebData CardData )
                        newHistory =
                            Dict.merge
                                (\id ( ts, cd ) -> List.append [ ( id, ts, cd ) ])
                                inBoth
                                (\id ( ts, cd ) -> List.append [ ( id, ts, cd ) ])
                                oldHistoryDict
                                newHistoryDict
                                []
                    in
                    CardBased data newHistory conflicts_

                Err err ->
                    model

        GitLike _ _ ->
            model


decodeHistory : Dec.Decoder (List ( String, Time.Posix, Maybe (List (Card String)) ))
decodeHistory =
    Dec.list <|
        Dec.map3 (\id ts cards -> ( id, ts, cards ))
            (Dec.field "snapshot" Dec.string)
            (Dec.field "ts" (Dec.map Time.millisToPosix Dec.int))
            (Dec.field "data" (Dec.maybe decodeCards))


getHistoryList : Model -> List ( String, Time.Posix, Maybe Tree )
getHistoryList model =
    case model of
        GitLike data _ ->
            let
                tripleFromCommit ( cid, c ) =
                    ( cid
                    , c.timestamp |> Time.millisToPosix
                    , checkoutCommit cid data
                    )
            in
            data
                |> .commits
                |> Dict.toList
                |> List.sortBy (\( cid, c ) -> c.timestamp)
                |> List.map tripleFromCommit

        CardBased _ history _ ->
            history
                |> List.map (\( id, ts, cardData_ ) -> ( id, ts, cardData_ |> RemoteData.toMaybe |> Maybe.map toTree ))
                |> List.reverse



---


intToBool : Dec.Decoder Bool
intToBool =
    Dec.map (\i -> i == 1) Dec.int


encodeMaybe : Maybe String -> Enc.Value
encodeMaybe maybe =
    case maybe of
        Just str ->
            Enc.string str

        Nothing ->
            Enc.null


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0
