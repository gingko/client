module Trees exposing (Model, TreeMsg(..), apply, conflictToTreeMsg, defaultModel, defaultTree, opToTreeMsg, renameNodes, setTree, setTreeWithConflicts, update, view)

import Diff exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2, lazy3)
import Html5.DragDrop as DragDrop
import List.Extra as ListExtra
import Markdown
import Regex
import Sha1 exposing (Diff, diff3Merge, sha1)
import TreeUtils exposing (getChildren, getColumns, getParent, getTree)
import Tuple exposing (first, second)
import Types exposing (..)



-- MODEL


type alias Model =
    { tree : Tree
    , columns : List Column
    }


defaultModel : Model
defaultModel =
    { tree = defaultTree
    , columns = [ [ [ defaultTree ] ], [ getChildren defaultTree ] ]
    }


defaultTree : Tree
defaultTree =
    { id = "0"
    , content = ""
    , children = Children [ Tree "1" "" (Children []) ]
    }



-- UPDATE


type TreeMsg
    = Nope
    | Ins String String String Int
    | Upd String String
    | Mov Tree String Int
    | Rmv String
    | Paste Tree String Int


update : TreeMsg -> Model -> Model
update msg model =
    setTree (updateTree msg model.tree) model


updateTree : TreeMsg -> Tree -> Tree
updateTree msg tree =
    case msg of
        Ins newId newContent parentId idx ->
            insertSubtree (Tree newId newContent (Children [])) parentId idx tree

        Upd id str ->
            modifyTree id (\t -> { t | content = str }) tree

        Mov newTree parentId idx ->
            tree
                |> pruneSubtree newTree.id
                |> insertSubtree newTree parentId idx

        Rmv id ->
            pruneSubtree id tree

        Paste newTree parentId idx ->
            tree
                |> insertSubtree newTree parentId idx

        Nope ->
            tree


setTree : Tree -> Model -> Model
setTree newTree model =
    let
        newColumns =
            if newTree /= model.tree then
                getColumns [ [ [ newTree ] ] ]

            else
                model.columns
    in
    { model
        | tree = newTree
        , columns = newColumns
    }


setTreeWithConflicts : List Conflict -> Tree -> Model -> Model
setTreeWithConflicts conflicts originalTree model =
    let
        newTree =
            originalTree
                |> apply (List.map (conflictToTreeMsg originalTree) conflicts)

        newColumns =
            if newTree /= model.tree then
                getColumns [ [ [ newTree ] ] ]

            else
                model.columns
    in
    { model
        | tree = newTree
        , columns = newColumns
    }


conflictToTreeMsg : Tree -> Conflict -> TreeMsg
conflictToTreeMsg tree ({ id, opA, opB, selection, resolved } as conflict) =
    case ( selection, resolved ) of
        ( Ours, False ) ->
            opToTreeMsg tree opA

        ( Theirs, False ) ->
            opToTreeMsg tree opB

        ( Manual, False ) ->
            case ( opA, opB ) of
                ( Mod tid _ strA orig, Mod _ _ strB _ ) ->
                    let
                        tokenize s =
                            Regex.split (Regex.fromString "(\\s+|\\b)" |> Maybe.withDefault Regex.never) s

                        -- List String
                        changeMerge d ds =
                            case ( d, ds ) of
                                ( NoChange a, (NoChange b) :: tail ) ->
                                    NoChange (a ++ b) :: tail

                                ( Added a, (Added b) :: tail ) ->
                                    Added (a ++ b) :: tail

                                ( Removed a, (Removed b) :: tail ) ->
                                    Removed (a ++ b) :: tail

                                ( ch, list ) ->
                                    ch :: list

                        diffWords l r =
                            diff (tokenize l) (tokenize r)
                                |> List.foldr changeMerge []
                                |> List.map
                                    (\c ->
                                        case c of
                                            NoChange s ->
                                                s

                                            Added s ->
                                                "{++" ++ s ++ "++}"

                                            Removed s ->
                                                "{--" ++ s ++ "--}"
                                    )
                                |> String.join ""

                        diffLinesString l r =
                            diffLines l r
                                |> List.map
                                    (\c ->
                                        case c of
                                            NoChange s ->
                                                s

                                            Added s ->
                                                "{++" ++ s ++ "++}"

                                            Removed s ->
                                                "{--" ++ s ++ "--}"
                                    )
                                |> String.join "\n"

                        mergedString : String
                        mergedString =
                            diff3Merge (String.lines strA) (String.lines orig) (String.lines strB)
                                |> List.map
                                    (\c ->
                                        case c of
                                            Sha1.DiffOk strings ->
                                                String.join "\n" strings

                                            Sha1.DiffConflict ( strAs, strOs, strBs ) ->
                                                "\n`>>>>>>>`\n"
                                                    ++ String.join "\n" strAs
                                                    ++ "\n`=======`\n"
                                                    ++ String.join "\n" strBs
                                                    ++ "\n`<<<<<<<`\n"
                                    )
                                |> String.join "\n"

                        manualString =
                            "`Your version:`\n"
                                ++ diffLinesString orig strA
                                ++ "\n\n--------\n`Their version:`\n"
                                ++ diffLinesString orig strB
                    in
                    Upd tid mergedString

                _ ->
                    Nope

        _ ->
            Nope


opToTreeMsg : Tree -> Op -> TreeMsg
opToTreeMsg origTree op =
    case op of
        Mod tid _ str _ ->
            Upd tid str

        Del tid _ ->
            Rmv tid

        Types.Ins id str pids idx ->
            case ListExtra.last pids of
                Just pid ->
                    Ins id str pid idx

                Nothing ->
                    Nope

        Types.Mov tid opids oidx npids nidx ->
            case ( getTree tid origTree, ListExtra.last npids ) of
                ( Just tree, Just pid ) ->
                    Mov tree pid nidx

                _ ->
                    Nope



-- TREE TRANSFORMATIONS


apply : List TreeMsg -> Tree -> Tree
apply msgs tree =
    List.foldl (\m t -> updateTree m t) tree msgs


insertSubtree : Tree -> String -> Int -> Tree -> Tree
insertSubtree subtree parentId idx tree =
    let
        fn =
            \c -> List.take idx c ++ [ subtree ] ++ List.drop idx c
    in
    modifyChildren parentId fn tree


pruneSubtree : String -> Tree -> Tree
pruneSubtree id tree =
    modifySiblings id (\c -> List.filter (\x -> x.id /= id) c) tree


modifyTree : String -> (Tree -> Tree) -> Tree -> Tree
modifyTree id upd tree =
    if tree.id == id then
        upd tree

    else
        { tree
            | children =
                getChildren tree
                    |> List.map (modifyTree id upd)
                    |> Children
        }


modifyChildren : String -> (List Tree -> List Tree) -> Tree -> Tree
modifyChildren pid upd tree =
    if tree.id == pid then
        { tree
            | children =
                getChildren tree
                    |> upd
                    |> Children
        }

    else
        { tree
            | children =
                getChildren tree
                    |> List.map (modifyChildren pid upd)
                    |> Children
        }


modifySiblings : String -> (List Tree -> List Tree) -> Tree -> Tree
modifySiblings id upd tree =
    case getParent id tree of
        Nothing ->
            tree

        Just parentTree ->
            modifyChildren parentTree.id upd tree


renameNodes : String -> Tree -> Tree
renameNodes salt tree =
    let
        newId =
            "node-" ++ sha1 (salt ++ tree.id)
    in
    { tree
        | id = newId
        , children =
            getChildren tree
                |> List.map (renameNodes salt)
                |> Children
    }



-- VIEW


view : ViewState -> Model -> Html Msg
view vstate model =
    let
        searchFilter term_ cols =
            case term_ of
                Just term ->
                    let
                        hasTerm tree =
                            term
                                |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
                                |> Maybe.withDefault Regex.never
                                |> (\t -> Regex.contains t tree.content)
                    in
                    cols
                        |> List.map (\c -> List.map (\g -> List.filter hasTerm g) c)

                Nothing ->
                    cols

        columnsWithDepth =
            model.columns
                |> searchFilter vstate.searchField
                |> List.indexedMap (\i c -> ( c, i ))
                |> List.drop 1

        getViewArgs cwd =
            let
                editing_ =
                    case vstate.editing of
                        Nothing ->
                            Nothing

                        Just editId ->
                            if first cwd |> List.concat |> List.map .id |> List.member editId then
                                Just editId

                            else
                                Nothing
            in
            VisibleViewState
                vstate.active
                editing_
                vstate.descendants
                vstate.ancestors
                vstate.dragModel
                vstate.collaborators

        columns =
            [ ( [ [] ], -1 ) ]
                ++ columnsWithDepth
                ++ [ ( [ [] ], List.length columnsWithDepth ) ]
                |> List.map (\t -> lazy3 viewColumn (getViewArgs t) (second t) (first t))
    in
    div
        [ id "app"
        ]
        columns


viewColumn : VisibleViewState -> Int -> Column -> Html Msg
viewColumn vstate depth col =
    let
        buffer =
            [ div [ class "buffer" ] [] ]
    in
    div
        [ class "column" ]
        (buffer
            ++ List.map (lazy3 viewGroup vstate depth) col
            ++ buffer
        )


viewGroup : VisibleViewState -> Int -> Group -> Html Msg
viewGroup vstate depth xs =
    let
        firstChild =
            xs
                |> List.head
                |> Maybe.withDefault defaultTree
                |> .id

        lastChild =
            xs
                |> List.reverse
                |> List.head
                |> Maybe.withDefault defaultTree
                |> .id

        hasActive =
            xs
                |> List.map .id
                |> List.member vstate.active

        isActiveDescendant =
            vstate.descendants
                |> List.member firstChild

        viewFunction t =
            let
                isActive =
                    t.id == vstate.active

                isAncestor =
                    List.member t.id vstate.ancestors

                isEditing =
                    case vstate.editing of
                        Just editId ->
                            t.id == editId

                        Nothing ->
                            False

                isLast =
                    t.id == lastChild

                collabsEditingCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == Editing t.id)
                        |> List.map .uid

                collabsOnCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == Active t.id || c.mode == Editing t.id)
                        |> List.map .uid
            in
            if t.id == vstate.active && not isEditing then
                ( t.id, viewCardActive t.id t.content (hasChildren t) isLast collabsOnCard collabsEditingCard )

            else if isEditing then
                ( t.id, viewCardEditing t.id t.content (hasChildren t) )

            else
                ( t.id, viewCardOther t.id t.content isEditing (hasChildren t) isAncestor isLast collabsOnCard collabsEditingCard vstate.dragModel )
    in
    Keyed.node "div"
        [ classList
            [ ( "group", True )
            , ( "has-active", hasActive )
            , ( "active-descendant", isActiveDescendant )
            ]
        ]
        (List.map viewFunction xs)


viewCardOther : String -> String -> Bool -> Bool -> Bool -> Bool -> List String -> List String -> DragDrop.Model String DropId -> Html Msg
viewCardOther cardId content isEditing isParent isAncestor isLast collabsOnCard collabsEditingCard dragModel =
    let
        dropRegions =
            let
                dropId_ =
                    DragDrop.getDropId dragModel

                dropDiv str dId =
                    div
                        ([ classList
                            [ ( "drop-region-" ++ str, True )
                            , ( "drop-hover", dropId_ == Just dId )
                            ]
                         ]
                            ++ DragDrop.droppable DragDropMsg dId
                        )
                        []
            in
            [ dropDiv "above" (Above cardId)
            , dropDiv "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDiv "below" (Below cardId) ]

                    else
                        []
                   )
    in
    div
        ([ id ("card-" ++ cardId)
         , dir "auto"
         , classList
            [ ( "card", True )
            , ( "ancestor", isAncestor )
            , ( "collab-active", not (List.isEmpty collabsOnCard) )
            , ( "collab-editing", not (List.isEmpty collabsEditingCard) )
            , ( "has-children", isParent )
            ]
         ]
            ++ (if not isEditing then
                    DragDrop.draggable DragDropMsg cardId

                else
                    []
               )
        )
        (dropRegions
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy viewContent content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardActive : String -> String -> Bool -> Bool -> List String -> List String -> Html Msg
viewCardActive cardId content isParent isLast collabsOnCard collabsEditingCard =
    let
        buttons =
            [ div [ class "flex-row card-top-overlay" ]
                [ span
                    [ class "card-btn ins-above"
                    , title "Insert Above (Ctrl+K)"
                    , onClick (InsertAbove cardId)
                    ]
                    [ text "+" ]
                ]
            , div [ class "flex-column card-right-overlay" ]
                [ span
                    [ class "card-btn delete"
                    , title "Delete Card (Ctrl+Backspace)"
                    , onClick (DeleteCard cardId)
                    ]
                    []
                , span
                    [ class "card-btn ins-right"
                    , title "Add Child (Ctrl+L)"
                    , onClick (InsertChild cardId)
                    ]
                    [ text "+" ]
                , span
                    [ class "card-btn edit"
                    , title "Edit Card (Enter)"
                    , onClick (OpenCard cardId content)
                    ]
                    []
                ]
            , div [ class "flex-row card-bottom-overlay" ]
                [ span
                    [ class "card-btn ins-below"
                    , title "Insert Below (Ctrl+J)"
                    , onClick (InsertBelow cardId)
                    ]
                    [ text "+" ]
                ]
            ]
    in
    div
        ([ id ("card-" ++ cardId)
         , dir "auto"
         , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "collab-active", not (List.isEmpty collabsOnCard) )
            , ( "collab-editing", not (List.isEmpty collabsEditingCard) )
            , ( "has-children", isParent )
            ]
         ]
            ++ DragDrop.draggable DragDropMsg cardId
        )
        (buttons
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy viewContent content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardEditing : String -> String -> Bool -> Html Msg
viewCardEditing cardId content isParent =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "editing", True )
            , ( "has-children", isParent )
            ]
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , value content
            ]
            []
        , div [ class "flex-column card-right-overlay" ]
            [ span
                [ class "card-btn save"
                , title "Save Changes (Ctrl+Enter)"
                , onClick (Port (Keyboard "mod+enter"))
                ]
                []
            ]
        ]



-- HELPERS


hasChildren : Tree -> Bool
hasChildren { children } =
    case children of
        Children c ->
            (c
                |> List.length
            )
                /= 0


viewContent : String -> Html Msg
viewContent content =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }

        processedContent =
            let
                openAddDiff =
                    Regex.fromString "{\\+\\+" |> Maybe.withDefault Regex.never

                closeAddDiff =
                    Regex.fromString "\\+\\+}" |> Maybe.withDefault Regex.never

                openDelDiff =
                    Regex.fromString "{--" |> Maybe.withDefault Regex.never

                closeDelDiff =
                    Regex.fromString "--}" |> Maybe.withDefault Regex.never
            in
            content
                |> Regex.replace openAddDiff (\_ -> "<ins class='diff'>")
                |> Regex.replace closeAddDiff (\_ -> "</ins>")
                |> Regex.replace openDelDiff (\_ -> "<del class='diff'>")
                |> Regex.replace closeDelDiff (\_ -> "</del>")
    in
    Markdown.toHtmlWith options
        []
        processedContent


collabsSpan : List String -> List String -> Html Msg
collabsSpan collabsOnCard collabsEditingCard =
    let
        collabsString =
            collabsOnCard
                |> List.map
                    (\c ->
                        if List.member c collabsEditingCard then
                            c ++ " is editing"

                        else
                            c
                    )
                |> String.join ", "
    in
    span [ class "collaborators" ] [ text collabsString ]
