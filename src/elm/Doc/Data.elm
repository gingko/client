module Doc.Data exposing (CommitObject, Model, checkout, conflictList, conflictSelection, empty, emptyData, getCommit, head, historyList, lastCommitTime, localSave, received, requestCommit, resolve, success)

import Coders exposing (treeToValue, tupleDecoder)
import Dict exposing (Dict)
import Diff3 exposing (diff3Merge)
import Doc.Data.Conflict exposing (Conflict, Op(..), Selection(..), conflictWithSha, opString)
import Doc.TreeStructure exposing (apply, opToMsg)
import Json.Decode as Dec
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe exposing (andThen)
import Set exposing (Set)
import Tuple exposing (second)
import Types exposing (CardTreeOp(..), Children(..), Tree)



-- MODEL


type Model
    = CardBased (List (Card String))
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
        CardBased _ ->
            Nothing

        GitLike data _ ->
            Dict.get id data.refs |> Maybe.map .value


historyList : String -> Model -> List String
historyList startingSha model =
    case model of
        CardBased _ ->
            []

        GitLike data _ ->
            (data
                |> .commits
                |> Dict.toList
                |> List.sortBy (\( cid, c ) -> c.timestamp)
                |> ListExtra.splitWhen (\( cid, c ) -> cid == startingSha)
                |> Maybe.map Tuple.first
                |> Maybe.withDefault []
                |> List.map Tuple.first
            )
                ++ [ startingSha ]


getCommit : String -> Model -> Maybe CommitObject
getCommit sha model =
    case model of
        CardBased _ ->
            Nothing

        GitLike data _ ->
            data
                |> .commits
                |> Dict.get sha


conflictList : Model -> List Conflict
conflictList model =
    case model of
        GitLike _ Nothing ->
            []

        GitLike _ (Just { conflicts }) ->
            conflicts

        CardBased _ ->
            []


checkout : String -> Model -> Maybe Tree
checkout commitSha model =
    case model of
        CardBased _ ->
            Nothing

        GitLike data _ ->
            checkoutCommit commitSha data


lastCommitTime : Model -> Maybe Int
lastCommitTime model =
    case model of
        CardBased _ ->
            Nothing

        GitLike data _ ->
            data.commits
                |> Dict.values
                |> List.map .timestamp
                |> List.sort
                |> List.reverse
                |> List.head



-- EXPOSED : Functions


received : Dec.Value -> ( Model, Tree ) -> Maybe { newModel : Model, newTree : Tree }
received json ( oldModel, oldTree ) =
    case Dec.decodeValue decode json of
        Ok (CardBasedIn cards) ->
            let
                newModel =
                    CardBased cards

                newTree =
                    cards
                        |> toTree
            in
            Just { newModel = newModel, newTree = newTree }

        Ok (GitLikeIn ( newData, Nothing )) ->
            { newModel = GitLike newData Nothing
            , newTree = checkoutRef "heads/master" newData |> Maybe.withDefault oldTree
            }
                |> Just

        Ok (GitLikeIn ( newData, Just ( _, confHead ) )) ->
            let
                localHead =
                    Dict.get "heads/master" newData.refs |> Maybe.withDefault confHead

                mergedModel =
                    merge localHead.value confHead.value oldTree newData
            in
            case mergedModel of
                ( data, Nothing ) ->
                    { newModel = GitLike data Nothing
                    , newTree = checkoutRef "heads/master" data |> Maybe.withDefault oldTree
                    }
                        |> Just

                ( data, Just cdata ) ->
                    { newModel = GitLike data (Just cdata)
                    , newTree = cdata.mergedTree
                    }
                        |> Just

        Err err ->
            let
                _ =
                    Debug.log "Error decoding received data" err
            in
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
                CardBased _ ->
                    model

                GitLike d cd_ ->
                    GitLike (updateData d) cd_

        Err err ->
            model


conflictSelection : String -> Selection -> Model -> Model
conflictSelection cid selection model =
    case model of
        CardBased _ ->
            model

        GitLike data (Just confInfo) ->
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
            GitLike data (Just { confInfo | conflicts = newConflicts })

        GitLike _ _ ->
            model


resolve : String -> Model -> Model
resolve cid model =
    case model of
        CardBased _ ->
            model

        GitLike _ Nothing ->
            model

        GitLike d (Just confInfo) ->
            let
                newConflicts =
                    List.filter (\c -> c.id /= cid) confInfo.conflicts
            in
            GitLike d (Just { confInfo | conflicts = newConflicts })



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
                        ( [], [ conflict opA opB Ours ] )

                    else
                        ( [ opA, opB ], [] )

                ( Del idA _, Mod idB pidsB _ _ ) ->
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

                ( Mov idA _ _ newParentsA _, Mov idB _ _ newParentsB _ ) ->
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


decode : Dec.Decoder DataIn
decode =
    Dec.oneOf
        [ decodeCards |> Dec.map CardBasedIn
        , decodeGitLike |> Dec.map GitLikeIn
        ]


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
        CardBased _ ->
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
        CardBased data ->
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

                CTIns id content parId idx ->
                    Enc.null

                CTRmv string ->
                    Enc.null

                CTMov string maybeString int ->
                    Enc.null

                CTMrg aId bId bool ->
                    Enc.null

                CTBlk tree string int ->
                    Enc.null

        GitLike gitData maybeConflictInfo ->
            Enc.null


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
