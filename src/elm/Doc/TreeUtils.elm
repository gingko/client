module Doc.TreeUtils exposing (ScrollPosition, centerlineIds, dictUpdate, getAncestors, getChildren, getColumn, getColumnById, getColumns, getContent, getDepth, getDescendants, getFirstCard, getFirstInColumn, getIndex, getLastInColumn, getNext, getNextInColumn, getParent, getPrev, getPrevInColumn, getPrevNext, getPrevNextInColumn, getScrollPositions, getSiblings, getTree, getTreeWithPosition, newLine, preorderTraversal, scrollPositionToValue, sha1, withIdTree)

import Dict exposing (..)
import Json.Encode as Enc
import List.Extra as ListExtra
import Maybe.Extra as MaybeExtra
import SHA1
import String
import Types exposing (Children(..), Column, Tree)



-- TRANSFORMATIONS


getColumns : List Column -> List Column
getColumns cols =
    let
        lastColumn =
            case ListExtra.last cols of
                Nothing ->
                    [ [] ]

                Just c ->
                    c

        hasChildren =
            lastColumn
                |> List.concat
                |> List.any (\x -> getChildren x /= [])

        nextColumn prevColumns =
            List.map getChildren (List.concat prevColumns)
    in
    if hasChildren then
        getColumns (cols ++ [ nextColumn lastColumn ])

    else
        cols



-- ACCESSORS


getTree : String -> Tree -> Maybe Tree
getTree id tree =
    if tree.id == id then
        Just tree

    else
        getChildren tree
            |> List.map (getTree id)
            |> List.filter (\m -> m /= Nothing)
            |> List.head
            |> Maybe.withDefault Nothing


getParent : String -> Tree -> Maybe Tree
getParent id tree =
    case tree.children of
        Children [] ->
            Nothing

        Children children ->
            if List.member id (List.map .id children) then
                Just tree

            else
                children
                    |> List.map (getParent id)
                    |> List.filter (\m -> m /= Nothing)
                    |> List.head
                    |> Maybe.withDefault Nothing


getChildren : Tree -> List Tree
getChildren x =
    case x.children of
        Children c ->
            c


getSiblings : String -> Tree -> List Tree
getSiblings id tree =
    if getChildren tree |> List.map .id |> List.member id then
        getChildren tree

    else
        List.concatMap (getSiblings id) (getChildren tree)


getColumn : Int -> Tree -> Maybe (List (List Tree))
getColumn n tree =
    let
        cols =
            getColumns [ [ [ tree ] ] ]
    in
    ListExtra.getAt n cols


getColumnById : String -> Tree -> Maybe Column
getColumnById id tree =
    let
        n =
            getDepth 0 tree id
    in
    getColumn n tree


getTreeWithPosition : String -> Tree -> Maybe ( Tree, String, Int )
getTreeWithPosition id tree =
    Maybe.map3
        (\t p i -> ( t, p, i ))
        (getTree id tree)
        (getParent id tree |> Maybe.map .id)
        (getIndex id tree)


getPrevNext : Int -> String -> Tree -> Maybe Tree
getPrevNext shift id tree =
    let
        siblings =
            getSiblings id tree

        idx =
            siblings
                |> List.map .id
                |> ListExtra.elemIndex id
    in
    case idx of
        Nothing ->
            Nothing

        Just i ->
            siblings
                |> ListExtra.getAt (i + shift)


getPrev : String -> Tree -> Maybe Tree
getPrev id tree =
    getPrevNext -1 id tree


getNext : String -> Tree -> Maybe Tree
getNext id tree =
    getPrevNext 1 id tree


getPrevNextInColumn : Int -> String -> Tree -> Maybe Tree
getPrevNextInColumn shift id tree =
    let
        column_ =
            getColumnById id tree
    in
    case column_ of
        Nothing ->
            Nothing

        Just col ->
            let
                idx =
                    col
                        |> List.concat
                        |> List.map .id
                        |> ListExtra.elemIndex id
            in
            case idx of
                Nothing ->
                    Nothing

                Just i ->
                    col
                        |> List.concat
                        |> ListExtra.getAt (i + shift)


getPrevInColumn : String -> Tree -> Maybe Tree
getPrevInColumn id tree =
    getPrevNextInColumn -1 id tree


getNextInColumn : String -> Tree -> Maybe Tree
getNextInColumn id tree =
    getPrevNextInColumn 1 id tree


getFirstInColumn : String -> Tree -> String
getFirstInColumn id tree =
    case getColumnById id tree of
        Nothing ->
            id

        Just c ->
            case
                c
                    |> List.concat
                    |> List.map .id
                    |> List.head
            of
                Nothing ->
                    id

                Just firstId ->
                    firstId


getLastInColumn : String -> Tree -> String
getLastInColumn id tree =
    case getColumnById id tree of
        Nothing ->
            id

        Just c ->
            case
                c
                    |> List.concat
                    |> List.map .id
                    |> List.reverse
                    |> List.head
            of
                Nothing ->
                    id

                Just firstId ->
                    firstId


getContent : String -> Tree -> String
getContent id tree =
    case getTree id tree of
        Nothing ->
            ""

        Just t ->
            t.content


getIndex : String -> Tree -> Maybe Int
getIndex id tree =
    getSiblings id tree
        |> List.map .id
        |> ListExtra.elemIndex id


getDescendants : Tree -> List Tree
getDescendants t =
    let
        children =
            getChildren t
    in
    if List.isEmpty children then
        []

    else
        children ++ List.concatMap getDescendants children


getAncestors : Tree -> Tree -> List Tree -> List Tree
getAncestors all target accum =
    let
        current =
            case List.head accum of
                Nothing ->
                    target

                Just t ->
                    t
    in
    case getParent current.id all of
        Nothing ->
            accum

        Just p ->
            getAncestors all target (p :: accum)


getDepth : Int -> Tree -> String -> Int
getDepth prev tree id =
    case tree.children of
        Children children ->
            if tree.id == id then
                prev

            else
                children
                    |> List.map (\a -> getDepth (prev + 1) a id)
                    |> List.maximum
                    |> Maybe.withDefault 0


getFirstCard : Tree -> Maybe Tree
getFirstCard tree =
    case tree.children of
        Children (fst :: _) ->
            Just fst

        Children [] ->
            Nothing



-- SPECIAL PROPERTIES


type ScrollPosition
    = Center String
    | Before String
    | After String
    | Between String String
    | None


scrollPositionToValue : ScrollPosition -> Enc.Value
scrollPositionToValue sp =
    case sp of
        Center id ->
            Enc.object
                [ ( "target", Enc.string id )
                , ( "position", Enc.string "Center" )
                ]

        Before id ->
            Enc.object
                [ ( "target", Enc.string id )
                , ( "position", Enc.string "Before" )
                ]

        After id ->
            Enc.object
                [ ( "target", Enc.string id )
                , ( "position", Enc.string "After" )
                ]

        Between id1 id2 ->
            Enc.object
                [ ( "target", Enc.string id1 )
                , ( "target2", Enc.string id2 )
                , ( "position", Enc.string "Between" )
                ]

        None ->
            Enc.object
                [ ( "position", Enc.string "None" )
                ]


getScrollPositions : Tree -> List String -> Tree -> List ( Int, ScrollPosition )
getScrollPositions activeTree activePastIds fullTree =
    let
        ancestors =
            getAncestors fullTree activeTree [] |> List.map .id

        descendants =
            getDescendants activeTree
                |> List.map .id

        historySortedTree =
            fullTree
                |> sortChildrenBy (\t -> ListExtra.elemIndex t.id activePastIds |> Maybe.withDefault 9999)
                |> (\t ->
                        getColumns [ [ [ t ] ] ]
                   )
                |> List.map List.concat
                |> List.concatMap (List.map .id)
                |> List.filter (\li -> List.member li descendants)

        preorderedIds =
            preorderTraversal fullTree

        preorderActiveIndex =
            preorderedIds
                |> ListExtra.elemIndex activeTree.id
                |> Maybe.withDefault 0

        filterOut refList origList =
            origList |> List.filter (\li -> not <| List.member li refList)

        ( afterIds, beforeIds ) =
            preorderedIds
                |> ListExtra.splitAt preorderActiveIndex
                |> Tuple.mapBoth (filterOut descendants) (filterOut descendants)
                |> Tuple.mapBoth (filterOut ancestors) (filterOut ancestors)
                |> Tuple.mapBoth (filterOut [ activeTree.id ]) (filterOut [ activeTree.id ])

        flatColumns =
            getColumns [ [ [ fullTree ] ] ]
                |> List.map List.concat
                |> List.map (List.map .id)

        centerActive col =
            ListExtra.find ((==) activeTree.id) col
                |> Maybe.map Center

        centerAncestor col =
            ListExtra.find (\i -> List.member i ancestors) col
                |> Maybe.map Center

        centerLastActiveDescendant col =
            ListExtra.find (\i -> List.member i col) historySortedTree
                |> Maybe.map Center

        centerOtherDescendants col =
            ListExtra.find (\i -> List.member i descendants) col
                |> Maybe.map Center

        befores col =
            ListExtra.find (\i -> List.member i beforeIds) col

        afters col =
            ListExtra.find (\i -> List.member i afterIds) (col |> List.reverse)

        beforeAndOrAfter col =
            case ( befores col, afters col ) of
                ( Just bef, Just aft ) ->
                    Between aft bef |> Just

                ( Just bef, Nothing ) ->
                    Before bef |> Just

                ( Nothing, Just aft ) ->
                    After aft |> Just

                ( Nothing, Nothing ) ->
                    Nothing
    in
    flatColumns
        |> List.indexedMap
            (\idx col ->
                ( idx
                , MaybeExtra.orListLazy
                    [ \() -> centerActive col
                    , \() -> centerAncestor col
                    , \() -> centerLastActiveDescendant col
                    , \() -> centerOtherDescendants col
                    , \() -> beforeAndOrAfter col
                    ]
                    |> Maybe.withDefault None
                )
            )
        |> List.drop 1


preorderTraversal : Tree -> List String
preorderTraversal tree =
    case tree.children of
        Children [] ->
            [ tree.id ]

        Children children ->
            tree.id :: List.concatMap preorderTraversal children


sortChildrenBy : (Tree -> comparable) -> Tree -> Tree
sortChildrenBy toComparable tree =
    let
        sortedChildren =
            case tree.children of
                Children [] ->
                    tree.children

                Children children ->
                    children
                        |> List.sortBy toComparable
                        |> List.map (sortChildrenBy toComparable)
                        |> Children
    in
    { tree | children = sortedChildren }


centerlineIds : List (List String) -> List String -> List String -> List (List String)
centerlineIds flatCols allIds activePast =
    let
        lastActiveOrAll aP ids =
            let
                lastActiveIdx_ =
                    aP
                        |> ListExtra.findIndex (\a -> List.member a ids)
            in
            case lastActiveIdx_ of
                Nothing ->
                    ids

                Just idx ->
                    aP
                        |> ListExtra.getAt idx
                        -- Maybe String
                        |> Maybe.withDefault "1"
                        |> List.singleton
    in
    flatCols
        |> List.drop 1
        |> List.map (\c -> List.filter (\id -> List.member id allIds) c)
        |> ListExtra.filterNot List.isEmpty
        |> List.map (lastActiveOrAll activePast)



-- HELPERS


newLine : String
newLine =
    String.fromList [ '\n' ]


withIdTree : String -> Tree
withIdTree id =
    Tree id "" (Children [])


sha1 : String -> String
sha1 str =
    str |> SHA1.fromString |> SHA1.toHex


dictUpdate : comparable -> (b -> b) -> Dict comparable b -> Dict comparable b
dictUpdate id upd dict =
    Dict.update
        id
        (\n_ ->
            case n_ of
                Just n ->
                    Just (upd n)

                Nothing ->
                    Nothing
        )
        dict
