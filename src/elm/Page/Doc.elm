module Page.Doc exposing (Model, Msg, ParentMsg(..), activate, checkoutCommit, incoming, init, saveAndStopEditing, saveCardIfEditing, subscriptions, update, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (treeToMarkdownOutline, treeToMarkdownString, treeToValue)
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, toDebouncer)
import Doc.Data as Data
import Doc.Data.Conflict exposing (Selection)
import Doc.Fonts as Fonts
import Doc.Fullscreen as Fullscreen
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils exposing (..)
import Doc.UI as UI exposing (countWords, viewConflict, viewMobileButtons, viewSearchField)
import GlobalData exposing (GlobalData)
import Html exposing (Attribute, Html, div, span, text, textarea, ul)
import Html.Attributes as Attributes exposing (attribute, class, classList, dir, id, style, title, value)
import Html.Events exposing (custom, onClick, onDoubleClick, onInput)
import Html.Extra exposing (viewIf)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3, lazy4, lazy7, lazy8)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import List.Extra as ListExtra
import Markdown
import Outgoing exposing (Msg(..), send)
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Random
import Regex
import Task
import Time
import Translation exposing (Language, TranslationId(..), tr)
import Types exposing (..)
import Utils exposing (randomPositiveInt)



-- MODEL


type alias Model =
    -- Document state
    { workingTree : TreeStructure.Model
    , data : Data.Model

    -- SPA Page State
    , globalData : GlobalData
    , loading : Bool
    , isExpired : Bool

    -- Transient state
    , viewState : ViewState
    , dirty : Bool
    , lastLocalSave : Maybe Time.Posix
    , lastRemoteSave : Maybe Time.Posix
    , field : String
    , textCursorInfo : TextCursorInfo
    , debouncerLocalSave : Debouncer () ()
    , debouncerStateCommit : Debouncer () ()
    , fileSearchField : String
    , wordcountTrayOpen : Bool
    , fontSelectorOpen : Bool

    -- Settings
    , uid : String
    , fonts : Fonts.Model
    , startingWordcount : Int
    }


init : Bool -> GlobalData -> Model
init isNew globalData =
    { workingTree = TreeStructure.defaultModel
    , data = Data.empty
    , globalData = globalData
    , loading = not isNew
    , isExpired = False
    , debouncerLocalSave =
        Debouncer.debounce (fromSeconds 0.5)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 0.5)
            |> toDebouncer
    , debouncerStateCommit =
        Debouncer.throttle (fromSeconds 3)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 3)
            |> toDebouncer
    , uid = "0"
    , viewState =
        { active = "1"
        , viewMode =
            if isNew then
                Editing

            else
                Normal
        , activePast = []
        , descendants = []
        , ancestors = [ "0" ]
        , searchField = Nothing
        , dragModel = ( DragDrop.init, DragExternalModel Nothing False )
        , draggedTree = Nothing
        , copiedTree = Nothing
        , clipboardTree = Nothing
        , collaborators = []
        }
    , dirty = False
    , lastLocalSave = Nothing
    , lastRemoteSave = Nothing
    , field = ""
    , textCursorInfo = { selected = False, position = End, text = ( "", "" ) }
    , fileSearchField = ""
    , wordcountTrayOpen = False
    , fontSelectorOpen = False
    , fonts = Fonts.default
    , startingWordcount = 0
    }



-- UPDATE


type Msg
    = NoOp
      -- === Card Activation ===
    | Activate String
    | SearchFieldUpdated String
      -- === Card Editing  ===
    | OpenCard String String
    | UpdateActiveField String String
    | AutoSave
    | SaveAndCloseCard
    | EditToFullscreenMode
    | FullscreenMsg Fullscreen.Msg
    | DeleteCard String
      -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
      -- === Dragging ===
    | DragDropMsg (DragDrop.Msg String DropId)
    | DragExternal DragExternalMsg
      -- === History ===
    | ThrottledLocalSave (Debouncer.Msg ())
    | LocalSave Time.Posix
    | ThrottledCommit (Debouncer.Msg ())
    | Commit Time.Posix
    | SetSelection String Selection String
    | Resolve String
      -- === UI ===
      -- Misc UI
    | FullscreenRequested
      -- === Ports ===
    | Pull
    | LogErr String


type ParentMsg
    = NoParentMsg
    | CloseTooltip
    | LocalSaveDo Time.Posix
    | CommitDo Time.Posix


type DragExternalMsg
    = DragEnter DropId
    | DragLeave DropId


updateDoc : Msg -> Model -> ( Model, Cmd Msg )
updateDoc msg ({ workingTree } as model) =
    let
        vs =
            model.viewState
    in
    case msg of
        -- === Card Activation ===
        Activate id ->
            ( model
            , Cmd.none
            )
                |> saveCardIfEditing
                |> activate id False

        SearchFieldUpdated inputField ->
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

                ( maybeBlur, newSearchField ) =
                    case inputField of
                        "" ->
                            ( \( m, c ) ->
                                ( m
                                , Cmd.batch [ c, Task.attempt (\_ -> NoOp) (Browser.Dom.blur "search-input") ]
                                )
                            , Nothing
                            )

                        str ->
                            ( identity
                            , Just str
                            )

                filteredCardIds =
                    searchFilter newSearchField model.workingTree.columns
                        |> List.map (\c -> List.map (\g -> List.map .id g) c)
                        |> List.concat
                        |> List.concat

                allCardsInOrder =
                    getDescendants model.workingTree.tree
                        |> List.map .id

                firstFilteredCardId_ =
                    ListExtra.find (\cId -> List.member cId filteredCardIds) allCardsInOrder

                maybeActivate =
                    case ( newSearchField, firstFilteredCardId_ ) of
                        ( Just _, Just id ) ->
                            activate id False

                        ( Nothing, _ ) ->
                            activate vs.active False

                        _ ->
                            identity
            in
            ( { model | viewState = { vs | searchField = newSearchField } }
            , Cmd.none
            )
                |> maybeBlur
                |> maybeActivate

        -- === Card Editing  ===
        OpenCard id str ->
            ( model
            , Cmd.none
            )
                |> openCard id str

        UpdateActiveField id str ->
            ( { model
                | field = str
                , dirty = True
              }
            , Cmd.batch
                [ send <| SetDirty True
                , send <| SetTextareaClone id str
                ]
            )
                |> localSave

        AutoSave ->
            ( model, Cmd.none ) |> saveCardIfEditing

        SaveAndCloseCard ->
            saveAndStopEditing model

        EditToFullscreenMode ->
            model |> enterFullscreen

        FullscreenMsg fullscreenMsg ->
            case fullscreenMsg of
                Fullscreen.OpenCard id str ->
                    ( model
                    , Cmd.none
                    )
                        |> saveCardIfEditing
                        |> openCardFullscreen id str

                Fullscreen.UpdateField id str ->
                    ( { model | dirty = True, field = str }
                    , send <| SetDirty True
                    )
                        |> localSave
                        |> (if vs.active /= id then
                                activate id False

                            else
                                identity
                           )

        DeleteCard id ->
            ( model
            , Cmd.none
            )
                |> deleteCard id

        -- === Card Insertion  ===
        InsertAbove id ->
            ( model
            , Cmd.none
            )
                |> insertAbove id ""

        InsertBelow id ->
            ( model
            , Cmd.none
            )
                |> insertBelow id ""

        InsertChild id ->
            ( model
            , Cmd.none
            )
                |> insertChild id ""

        -- === Card Moving  ===
        DragDropMsg dragDropMsg ->
            let
                ( newDragModel, dragResult_ ) =
                    DragDrop.update dragDropMsg (Tuple.first vs.dragModel)

                modelDragUpdated =
                    { model
                        | viewState =
                            { vs
                                | dragModel = ( newDragModel, Tuple.second vs.dragModel )
                            }
                    }
            in
            case ( DragDrop.getDragId newDragModel, dragResult_ ) of
                ( Just _, Nothing ) ->
                    -- Dragging
                    ( modelDragUpdated
                    , DragDrop.getDragstartEvent dragDropMsg
                        |> Maybe.map .event
                        |> Maybe.map (\json -> send <| DragStart json)
                        |> Maybe.withDefault Cmd.none
                    )

                ( Nothing, Just ( _, dropId, _ ) ) ->
                    -- Drop success
                    case vs.draggedTree of
                        Just ( draggedTree, _, _ ) ->
                            let
                                moveOperation =
                                    case dropId of
                                        Into id ->
                                            move draggedTree id 999999

                                        Above id ->
                                            move draggedTree
                                                ((getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0")
                                                ((getIndex id model.workingTree.tree |> Maybe.withDefault 0) |> Basics.max 0)

                                        Below id ->
                                            move draggedTree
                                                ((getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0")
                                                ((getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1)
                            in
                            ( { modelDragUpdated | viewState = { vs | draggedTree = Nothing }, dirty = True }, Cmd.batch [ send <| SetDirty True, send <| DragDone ] )
                                |> moveOperation

                        Nothing ->
                            ( modelDragUpdated, Cmd.none )

                ( Nothing, Nothing ) ->
                    -- NotDragging
                    case vs.draggedTree of
                        Just ( draggedTree, parentId, idx ) ->
                            ( modelDragUpdated, Cmd.none )
                                |> move draggedTree parentId idx

                        Nothing ->
                            ( modelDragUpdated, Cmd.none )

                ( Just _, Just _ ) ->
                    -- Should be Impossible: both Dragging and Dropped
                    ( modelDragUpdated, Cmd.none )

        DragExternal dragExternalMsg ->
            case dragExternalMsg of
                DragEnter dId ->
                    ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Just dId, isDragging = True } ) } }, Cmd.none )

                DragLeave dId ->
                    if (Tuple.second vs.dragModel |> .dropId) == Just dId then
                        ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = True } ) } }, Cmd.none )

                    else
                        ( model, Cmd.none )

        -- === History ===
        ThrottledLocalSave subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerLocalSave

                mappedCmd =
                    Cmd.map ThrottledLocalSave subCmd

                updatedModel =
                    { model | debouncerLocalSave = subModel }
            in
            case emitted_ of
                Just () ->
                    ( updatedModel
                    , Cmd.batch [ Task.perform LocalSave Time.now, mappedCmd ]
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        LocalSave time ->
            ( model, Cmd.none )

        ThrottledCommit subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerStateCommit

                mappedCmd =
                    Cmd.map ThrottledCommit subCmd

                updatedModel =
                    { model | debouncerStateCommit = subModel }
            in
            case emitted_ of
                Just () ->
                    ( updatedModel
                    , Cmd.batch [ Task.perform Commit Time.now, mappedCmd ]
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        Commit time ->
            ( model, Cmd.none )

        SetSelection cid selection id ->
            let
                newData =
                    Data.conflictSelection cid selection model.data
            in
            ( { model
                | data = newData
                , workingTree = TreeStructure.setTreeWithConflicts (Data.conflictList newData) model.workingTree.tree model.workingTree
              }
            , Cmd.none
            )
                |> activate id True

        Resolve cid ->
            let
                newData =
                    Data.resolve cid model.data
            in
            ( { model
                | data = newData
                , workingTree = TreeStructure.setTreeWithConflicts (Data.conflictList newData) model.workingTree.tree model.workingTree
              }
            , Cmd.none
            )
                |> localSave
                |> addToHistory

        -- === UI ===
        FullscreenRequested ->
            ( model, send <| RequestFullscreen )

        -- === Ports ===
        Pull ->
            ( model, send <| PullData )

        LogErr _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model
            , Cmd.none
            )


update : Msg -> Model -> ( Model, Cmd Msg, ParentMsg )
update msg model =
    case msg of
        Commit commitTime ->
            updateDoc msg model |> parentMsg (CommitDo commitTime)

        LocalSave saveTime ->
            updateDoc msg model |> parentMsg (LocalSaveDo saveTime)

        _ ->
            updateDoc msg model |> parentMsg NoParentMsg


parentMsg : ParentMsg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg, ParentMsg )
parentMsg pMsg ( model, cmd ) =
    ( model, cmd, pMsg )


incoming : Incoming.Msg -> Model -> ( Model, Cmd Msg )
incoming incomingMsg model =
    let
        vs =
            model.viewState
    in
    case incomingMsg of
        -- === Dialogs, Menus, Window State ===
        CancelCardConfirmed ->
            ( { model | dirty = False }
            , send <| SetDirty False
            )
                |> cancelCard

        -- === Database ===
        DataSaved dataIn ->
            let
                newData =
                    Data.success dataIn model.data
            in
            ( { model
                | data = newData
                , lastLocalSave = Data.lastCommitTime newData |> Maybe.map Time.millisToPosix
                , dirty = False
              }
            , send <| SetDirty False
            )

        DataReceived dataIn ->
            dataReceived dataIn model

        NotFound ->
            ( model, Cmd.none )

        MetadataSynced _ ->
            ( model, Cmd.none )

        MetadataSaved _ ->
            ( model, Cmd.none )

        MetadataSaveError ->
            ( model, Cmd.none )

        SavedRemotely time ->
            ( { model
                | lastRemoteSave = Just time
              }
            , Cmd.none
            )

        -- === Desktop ===
        SavedToFile _ _ ->
            ( model, Cmd.none )

        ClickedExport ->
            ( model, Cmd.none )

        -- === DOM ===
        DragStarted dragId ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Rmv dragId) model.workingTree

                draggedTree =
                    getTreeWithPosition dragId model.workingTree.tree
            in
            if List.isEmpty <| getChildren newTree.tree then
                ( model, Cmd.none )

            else
                ( { model | workingTree = newTree, viewState = { vs | draggedTree = draggedTree } }, Cmd.none )

        DragExternalStarted ->
            if model.viewState.viewMode == Normal then
                ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = True } ) } }, Cmd.none )

            else
                ( model, Cmd.none )

        DropExternal dropText ->
            case Tuple.second vs.dragModel |> .dropId of
                Just dropId ->
                    let
                        modelNoDrag =
                            { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = False } ) } }

                        baseModelCmdTuple =
                            case dropId of
                                Above cardId ->
                                    ( modelNoDrag, Cmd.none ) |> insertAbove cardId dropText

                                Into cardId ->
                                    ( modelNoDrag, Cmd.none ) |> insertChild cardId dropText

                                Below cardId ->
                                    ( modelNoDrag, Cmd.none ) |> insertBelow cardId dropText
                    in
                    baseModelCmdTuple
                        |> closeCard
                        |> localSave
                        |> addToHistory

                Nothing ->
                    ( model, Cmd.none )

        FullscreenChanged isFullscreen ->
            if vs.viewMode == FullscreenEditing && not isFullscreen then
                exitFullscreen model

            else
                ( model, Cmd.none )

        Paste tree ->
            normalMode model (pasteBelow vs.active tree)

        PasteInto tree ->
            normalMode model (pasteInto vs.active tree)

        FieldChanged str ->
            ( { model | field = str, dirty = True }, Cmd.none )

        TextCursor textCursorInfo ->
            if model.textCursorInfo /= textCursorInfo then
                ( { model | textCursorInfo = textCursorInfo }
                , Cmd.none
                )

            else
                ( model, Cmd.none )

        ClickedOutsideCard ->
            if model.viewState.viewMode == Editing then
                ( model, Cmd.none )
                    |> saveCardIfEditing
                    |> closeCard

            else
                ( model, Cmd.none )

        CheckboxClicked cardId checkboxNumber ->
            case getTree cardId model.workingTree.tree of
                Nothing ->
                    ( model, Cmd.none )

                Just originalCard ->
                    let
                        checkboxes =
                            Regex.fromStringWith { caseInsensitive = True, multiline = True }
                                "\\[(x| )\\]"
                                |> Maybe.withDefault Regex.never

                        checkboxReplacer { match, number } =
                            case ( number == checkboxNumber, match ) of
                                ( True, "[ ]" ) ->
                                    "[X]"

                                ( True, "[x]" ) ->
                                    "[ ]"

                                ( True, "[X]" ) ->
                                    "[ ]"

                                _ ->
                                    match

                        newContent =
                            originalCard.content
                                |> Regex.replace checkboxes checkboxReplacer

                        newTree =
                            TreeStructure.update (TreeStructure.Upd cardId newContent) model.workingTree
                    in
                    ( { model | workingTree = newTree, dirty = True }, Cmd.none )
                        |> localSave
                        |> addToHistory

        -- === UI ===
        FontSelectorOpen fonts ->
            ( { model | fonts = Fonts.setSystem fonts model.fonts, fontSelectorOpen = True }
            , Cmd.none
            )

        Keyboard shortcut ->
            case shortcut of
                "shift+enter" ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Cmd.none
                            )
                                |> openCardFullscreen vs.active (getContent vs.active model.workingTree.tree)

                        _ ->
                            ( model, Cmd.none )

                "mod+enter" ->
                    saveAndStopEditing model

                "mod+s" ->
                    saveCardIfEditing ( model, Cmd.none )

                "enter" ->
                    normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

                "mod+backspace" ->
                    normalMode model (deleteCard vs.active)

                "esc" ->
                    model |> intentCancelCard

                "mod+j" ->
                    if model.viewState.viewMode == Normal then
                        insertBelow vs.active "" ( model, Cmd.none )

                    else
                        let
                            ( beforeText, afterText ) =
                                model.textCursorInfo.text
                        in
                        ( { model | field = beforeText }
                        , Cmd.none
                        )
                            |> saveCardIfEditing
                            |> insertBelow vs.active afterText
                            |> setCursorPosition 0

                "mod+down" ->
                    normalMode model (insertBelow vs.active "")

                "mod+k" ->
                    if model.viewState.viewMode == Normal then
                        insertAbove vs.active "" ( model, Cmd.none )

                    else
                        let
                            ( beforeText, afterText ) =
                                model.textCursorInfo.text
                        in
                        ( { model | field = afterText }
                        , Cmd.none
                        )
                            |> saveCardIfEditing
                            |> insertAbove vs.active beforeText

                "mod+up" ->
                    normalMode model (insertAbove vs.active "")

                "mod+l" ->
                    let
                        ( beforeText, afterText ) =
                            model.textCursorInfo.text
                    in
                    ( { model | field = beforeText }
                    , Cmd.none
                    )
                        |> saveCardIfEditing
                        |> insertChild vs.active afterText
                        |> setCursorPosition 0

                "mod+right" ->
                    normalMode model (insertChild vs.active "")

                "mod+shift+j" ->
                    normalMode model (mergeDown vs.active)

                "mod+shift+down" ->
                    normalMode model (mergeDown vs.active)

                "mod+shift+k" ->
                    normalMode model (mergeUp vs.active)

                "mod+shift+up" ->
                    normalMode model (mergeUp vs.active)

                "h" ->
                    normalMode model (goLeft vs.active)

                "left" ->
                    normalMode model (goLeft vs.active)

                "j" ->
                    normalMode model (goDown vs.active)

                "down" ->
                    case vs.viewMode of
                        Normal ->
                            ( model, Cmd.none )
                                |> goDown vs.active

                        FullscreenEditing ->
                            {- check if at end
                               if so, getNextInColumn and openCardFullscreen it
                            -}
                            ( model, Cmd.none )

                        Editing ->
                            ( model, Cmd.none )

                "k" ->
                    normalMode model (goUp vs.active)

                "up" ->
                    normalMode model (goUp vs.active)

                "l" ->
                    normalMode model (goRight vs.active)

                "right" ->
                    normalMode model (goRight vs.active)

                "alt+up" ->
                    normalMode model (moveWithin vs.active -1)

                "alt+k" ->
                    normalMode model (moveWithin vs.active -1)

                "alt+down" ->
                    normalMode model (moveWithin vs.active 1)

                "alt+j" ->
                    normalMode model (moveWithin vs.active 1)

                "alt+left" ->
                    normalMode model (moveLeft vs.active)

                "alt+h" ->
                    normalMode model (moveLeft vs.active)

                "alt+right" ->
                    normalMode model (moveRight vs.active)

                "alt+l" ->
                    normalMode model (moveRight vs.active)

                "alt+shift+up" ->
                    normalMode model (moveWithin vs.active -5)

                "alt+shift+down" ->
                    normalMode model (moveWithin vs.active 5)

                "alt+pageup" ->
                    normalMode model (moveWithin vs.active -999999)

                "alt+pagedown" ->
                    normalMode model (moveWithin vs.active 999999)

                "home" ->
                    normalMode model (goToTopOfColumn vs.active)

                "end" ->
                    normalMode model (goToBottomOfColumn vs.active)

                "pageup" ->
                    normalMode model (goToTopOfGroup vs.active True)

                "pagedown" ->
                    normalMode model (goToBottomOfGroup vs.active True)

                "mod+x" ->
                    normalMode model (cut vs.active)

                "mod+c" ->
                    normalMode model (copy vs.active)

                "mod+b" ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , send (TextSurround vs.active "**")
                            )

                "mod+i" ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , send (TextSurround vs.active "*")
                            )

                "/" ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-input")
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )

                "w" ->
                    {--
                            case ( vs.viewMode, model.modalState ) of
                                ( Normal, NoModal ) ->
                                    ( { model | modalState = Wordcount }, Cmd.none )

                                ( Normal, Wordcount ) ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                _ ->
                                    --}
                    ( model, Cmd.none )

                "?" ->
                    {--
                            case ( vs.viewMode, model.modalState ) of
                                ( Normal, HelpScreen ) ->
                                    ( { model | modalState = NoModal }, Cmd.none )

                                ( Normal, _ ) ->
                                    ( { model | modalState = HelpScreen }, Cmd.none )

                                _ ->
                                    ( model, Cmd.none )
                            --}
                    ( model, Cmd.none )

                _ ->
                    ( model
                    , Cmd.none
                    )

        WillPrint ->
            ( model, Cmd.none )

        -- === Misc ===
        RecvCollabState collabState ->
            let
                newCollabs =
                    if List.member collabState.uid (vs.collaborators |> List.map .uid) then
                        vs.collaborators
                            |> List.map
                                (\c ->
                                    if c.uid == collabState.uid then
                                        collabState

                                    else
                                        c
                                )

                    else
                        collabState :: vs.collaborators

                newTree =
                    case collabState.mode of
                        CollabEditing editId ->
                            TreeStructure.update (TreeStructure.Upd editId collabState.field) model.workingTree

                        _ ->
                            model.workingTree
            in
            ( { model
                | workingTree = newTree
                , viewState = { vs | collaborators = newCollabs }
              }
            , Cmd.none
            )

        CollaboratorDisconnected uid ->
            ( { model
                | viewState =
                    { vs | collaborators = vs.collaborators |> List.filter (\c -> c.uid /= uid) }
              }
            , Cmd.none
            )

        -- === INTEGRATION TEST HOOKS ===
        TestTextImportLoaded _ ->
            ( model, Cmd.none )



-- === Card Activation ===


activate : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
activate tryId instant ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    if tryId == "0" then
        ( model
        , prevCmd
        )

    else
        let
            activeTree__ =
                getTree tryId model.workingTree.tree

            activeTree_ =
                case activeTree__ of
                    Just aTree ->
                        Just aTree

                    Nothing ->
                        getFirstCard model.workingTree.tree
        in
        case activeTree_ of
            Nothing ->
                ( model, prevCmd )

            Just activeTree ->
                let
                    newPast =
                        if tryId == vs.active then
                            vs.activePast

                        else
                            vs.active :: vs.activePast |> List.take 40

                    id =
                        activeTree.id

                    desc =
                        activeTree
                            |> getDescendants
                            |> List.map .id

                    anc =
                        getAncestors model.workingTree.tree activeTree []
                            |> List.map .id

                    newField =
                        activeTree.content

                    newModel =
                        { model
                            | viewState =
                                { vs
                                    | active = id
                                    , activePast = newPast
                                    , descendants = desc
                                    , ancestors = anc
                                }
                            , field = newField
                        }
                in
                case vs.viewMode of
                    FullscreenEditing ->
                        ( newModel
                        , Cmd.batch [ prevCmd, send <| ScrollFullscreenCards id ]
                        )

                    _ ->
                        let
                            scrollPositions =
                                getScrollPositions activeTree newPast model.workingTree.tree

                            colIdx =
                                getDepth 0 model.workingTree.tree activeTree.id
                        in
                        ( newModel
                        , Cmd.batch
                            [ prevCmd
                            , send
                                (ScrollCards (id :: newPast) scrollPositions colIdx instant)
                            ]
                        )


goLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goLeft id ( model, prevCmd ) =
    let
        targetId =
            getParent id model.workingTree.tree |> Maybe.withDefault defaultTree |> .id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goDown : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goDown id ( model, prevCmd ) =
    let
        targetId =
            case getNextInColumn id model.workingTree.tree of
                Nothing ->
                    id

                Just ntree ->
                    ntree.id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goUp : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goUp id ( model, prevCmd ) =
    let
        targetId =
            case getPrevInColumn id model.workingTree.tree of
                Nothing ->
                    id

                Just ntree ->
                    ntree.id
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goRight id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        tree_ =
            getTree id model.workingTree.tree

        childrenIds =
            getChildren (tree_ |> Maybe.withDefault defaultTree)
                |> List.map .id

        firstChildId =
            childrenIds
                |> List.head
                |> Maybe.withDefault id

        prevActiveOfChildren =
            vs.activePast
                |> List.filter (\a -> List.member a childrenIds)
                |> List.head
                |> Maybe.withDefault firstChildId
    in
    case tree_ of
        Nothing ->
            ( model
            , prevCmd
            )

        Just _ ->
            if List.length childrenIds == 0 then
                ( model
                , prevCmd
                )

            else
                ( model
                , prevCmd
                )
                    |> activate prevActiveOfChildren False



-- === Card Editing  ===


saveAndStopEditing : Model -> ( Model, Cmd Msg )
saveAndStopEditing model =
    let
        vs =
            model.viewState
    in
    case vs.viewMode of
        Normal ->
            ( model, Cmd.none ) |> openCard vs.active (getContent vs.active model.workingTree.tree)

        Editing ->
            ( model, Cmd.none )
                |> saveCardIfEditing
                |> closeCard

        FullscreenEditing ->
            ( model, Cmd.none )
                |> saveCardIfEditing
                |> closeCard
                |> activate model.viewState.active True


saveCardIfEditing : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saveCardIfEditing ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    case vs.viewMode of
        Normal ->
            ( model
            , prevCmd
            )

        _ ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Upd vs.active model.field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                  }
                , prevCmd
                )
                    |> localSave
                    |> addToHistory

            else
                ( { model | dirty = False }
                , Cmd.batch [ prevCmd, send <| SetDirty False ]
                )


openCard : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCard id str ( model, prevCmd ) =
    let
        vs =
            model.viewState

        ( newViewMode, maybeScroll ) =
            if vs.viewMode == FullscreenEditing then
                ( FullscreenEditing, send <| ScrollFullscreenCards id )

            else
                ( Editing, Cmd.none )

        isLocked =
            vs.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)

        isHistoryView =
            {--TODO: case model.headerMenu of
                HistoryView _ ->
                    True

                _ ->
                    --}
            False
    in
    if isHistoryView then
        ( model
        , Cmd.batch [ prevCmd, send (Alert "Cannot edit while browsing version history.") ]
        )

    else if isLocked then
        ( model
        , Cmd.batch [ prevCmd, send (Alert "Card is being edited by someone else.") ]
        )

    else
        ( { model
            | viewState = { vs | active = id, viewMode = newViewMode }
            , field = str
          }
        , Cmd.batch [ prevCmd, focus id, maybeScroll ]
        )


openCardFullscreen : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCardFullscreen id str ( model, prevCmd ) =
    ( model, prevCmd )
        |> openCard id str
        |> (\( m, c ) ->
                let
                    vs =
                        m.viewState
                in
                ( { m | viewState = { vs | active = id, viewMode = FullscreenEditing }, field = str }
                , Cmd.batch [ c, focus id ]
                )
           )


enterFullscreen : Model -> ( Model, Cmd Msg )
enterFullscreen model =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = FullscreenEditing } }
    , focus vs.active
    )


exitFullscreen : Model -> ( Model, Cmd Msg )
exitFullscreen model =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = Editing } }
    , Cmd.batch [ send <| SetField vs.active model.field, focus vs.active ]
    )


closeCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
closeCard ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    ( { model | viewState = { vs | viewMode = Normal }, field = "" }, prevCmd )


deleteCard : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
deleteCard id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        isLocked =
            vs.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)

        parent_ =
            getParent id model.workingTree.tree

        prev_ =
            getPrevInColumn id model.workingTree.tree

        next_ =
            getNextInColumn id model.workingTree.tree

        ( nextToActivate, isLastChild ) =
            case ( parent_, prev_, next_ ) of
                ( _, Just prev, _ ) ->
                    ( prev.id, False )

                ( _, Nothing, Just next ) ->
                    ( next.id, False )

                ( Just parent, Nothing, Nothing ) ->
                    ( parent.id, parent.id == "0" )

                ( Nothing, Nothing, Nothing ) ->
                    ( "0", True )
    in
    if isLocked then
        ( model
        , send (Alert "Card is being edited by someone else.")
        )

    else if isLastChild then
        ( model
        , send (Alert "Cannot delete last card.")
        )

    else
        ( { model
            | workingTree = TreeStructure.update (TreeStructure.Rmv id) model.workingTree
            , dirty = True
          }
        , Cmd.batch [ prevCmd, send <| SetDirty True ]
        )
            |> activate nextToActivate False
            |> localSave
            |> addToHistory


goToTopOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToTopOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getFirstInColumn id model.workingTree.tree) False


goToBottomOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToBottomOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getLastInColumn id model.workingTree.tree) False


goToTopOfGroup : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToTopOfGroup id fallToNextGroup ( model, prevCmd ) =
    let
        topSibling =
            case
                getSiblings id model.workingTree.tree
                    |> List.head
            of
                Nothing ->
                    id

                Just lastSiblingTree ->
                    lastSiblingTree.id

        targetId =
            if topSibling == id && fallToNextGroup then
                case getPrevInColumn id model.workingTree.tree of
                    Nothing ->
                        topSibling

                    Just previousColumnTree ->
                        previousColumnTree.id

            else
                topSibling
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


goToBottomOfGroup : String -> Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToBottomOfGroup id fallToNextGroup ( model, prevCmd ) =
    let
        bottomSibling =
            case
                getSiblings id model.workingTree.tree
                    |> List.reverse
                    |> List.head
            of
                Nothing ->
                    id

                Just lastSiblingTree ->
                    lastSiblingTree.id

        targetId =
            if bottomSibling == id && fallToNextGroup then
                case getNextInColumn id model.workingTree.tree of
                    Nothing ->
                        bottomSibling

                    Just nextColumnTree ->
                        nextColumnTree.id

            else
                bottomSibling
    in
    ( model
    , prevCmd
    )
        |> activate targetId False


cancelCard : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cancelCard ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    ( { model
        | viewState = { vs | viewMode = Normal }
        , field = ""
      }
    , prevCmd
    )
        |> activate vs.active True


intentCancelCard : Model -> ( Model, Cmd Msg )
intentCancelCard model =
    let
        vs =
            model.viewState

        originalContent =
            getContent vs.active model.workingTree.tree
    in
    case vs.viewMode of
        Normal ->
            ( model
            , Cmd.none
            )

        _ ->
            ( model
            , send (ConfirmCancelCard vs.active originalContent (tr (GlobalData.language model.globalData) AreYouSureCancel))
            )



-- === Card Insertion  ===


insert : String -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insert pid pos initText ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (GlobalData.seed model.globalData)

        newIdString =
            "node-" ++ (newId |> String.fromInt)
    in
    if not model.isExpired then
        ( { model
            | workingTree = TreeStructure.update (TreeStructure.Ins newIdString initText pid pos) model.workingTree
            , globalData = GlobalData.setSeed newSeed model.globalData
          }
        , prevCmd
        )
            |> openCard newIdString initText
            |> activate newIdString False

    else
        ( model, send <| Alert "Your Free Trial is Over.\n\nYou can view and edit your existing cards, but cannot create new ones." )


insertRelative : String -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertRelative id delta initText ( model, prevCmd ) =
    let
        idx =
            getIndex id model.workingTree.tree |> Maybe.withDefault 999999

        pid_ =
            getParent id model.workingTree.tree |> Maybe.map .id
    in
    case pid_ of
        Just pid ->
            ( model
            , prevCmd
            )
                |> insert pid (idx + delta) initText

        Nothing ->
            ( model
            , prevCmd
            )


insertAbove : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertAbove id initText tup =
    insertRelative id 0 initText tup


insertBelow : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertBelow id initText ( model, prevCmd ) =
    insertRelative id 1 initText ( model, prevCmd )


insertChild : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertChild id initText ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> insert id 999999 initText


mergeUp : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mergeUp id ( model, prevCmd ) =
    let
        currentTree_ =
            getTree id model.workingTree.tree

        prevTree_ =
            getPrevInColumn id model.workingTree.tree
    in
    case ( currentTree_, prevTree_ ) of
        ( Just currentTree, Just prevTree ) ->
            let
                mergedTree =
                    model.workingTree
                        |> TreeStructure.update (TreeStructure.Mrg currentTree prevTree True)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate prevTree.id False
                |> localSave
                |> addToHistory

        _ ->
            ( model, prevCmd )


mergeDown : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
mergeDown id ( model, prevCmd ) =
    let
        currentTree_ =
            getTree id model.workingTree.tree

        nextTree_ =
            getNextInColumn id model.workingTree.tree
    in
    case ( currentTree_, nextTree_ ) of
        ( Just currentTree, Just nextTree ) ->
            let
                mergedTree =
                    model.workingTree
                        |> TreeStructure.update (TreeStructure.Mrg currentTree nextTree False)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate nextTree.id False
                |> localSave
                |> addToHistory

        _ ->
            ( model, prevCmd )


setCursorPosition : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setCursorPosition pos ( model, prevCmd ) =
    ( model, Cmd.batch [ prevCmd, send (SetCursorPosition pos) ] )



-- === Card Moving  ===


move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Mov subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> activate subtree.id False
        |> localSave
        |> addToHistory


moveWithin : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveWithin id delta ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        pid_ =
            getParent id model.workingTree.tree
                |> Maybe.map .id

        refIdx_ =
            getIndex id model.workingTree.tree
    in
    case ( tree_, pid_, refIdx_ ) of
        ( Just tree, Just pid, Just refIdx ) ->
            ( model
            , prevCmd
            )
                |> move tree pid (refIdx + delta |> Basics.max 0)

        _ ->
            ( model
            , prevCmd
            )


moveLeft : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveLeft id ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        parentId =
            getParent id model.workingTree.tree
                |> Maybe.map .id
                |> Maybe.withDefault "invalid"

        parentIdx_ =
            getIndex parentId model.workingTree.tree

        grandparentId_ =
            getParent parentId model.workingTree.tree
                |> Maybe.map .id
    in
    case ( tree_, grandparentId_, parentIdx_ ) of
        ( Just tree, Just gpId, Just refIdx ) ->
            ( model
            , prevCmd
            )
                |> move tree gpId (refIdx + 1)

        _ ->
            ( model
            , prevCmd
            )


moveRight : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
moveRight id ( model, prevCmd ) =
    let
        tree_ =
            getTree id model.workingTree.tree

        prev_ =
            getPrev id model.workingTree.tree
                |> Maybe.map .id
    in
    case ( tree_, prev_ ) of
        ( Just tree, Just prev ) ->
            ( model
            , prevCmd
            )
                |> move tree prev 999999

        _ ->
            ( model
            , prevCmd
            )



-- === Card Cut/Copy/Paste ===


cut : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
cut id ( model, prevCmd ) =
    let
        parent_ =
            getParent id model.workingTree.tree

        prev_ =
            getPrevInColumn id model.workingTree.tree

        next_ =
            getNextInColumn id model.workingTree.tree

        isLastChild =
            case ( parent_, prev_, next_ ) of
                ( Just parent, Nothing, Nothing ) ->
                    parent.id == "0"

                _ ->
                    False
    in
    if isLastChild then
        ( model
        , send (Alert "Cannot cut last card")
        )

    else
        ( model, prevCmd )
            |> copy id
            |> deleteCard id


copy : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
copy id ( model, prevCmd ) =
    let
        vs =
            model.viewState

        copiedTree_ =
            getTree id model.workingTree.tree
    in
    ( { model
        | viewState = { vs | clipboardTree = copiedTree_ }
      }
    , Cmd.batch
        [ prevCmd
        , case copiedTree_ of
            Just tree ->
                send <| CopyCurrentSubtree <| treeToValue tree

            Nothing ->
                Cmd.none
        ]
    )


paste : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
paste subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Paste subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> activate subtree.id False
        |> localSave
        |> addToHistory


pasteBelow : String -> Tree -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteBelow id copiedTree ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (GlobalData.seed model.globalData)

        treeToPaste =
            TreeStructure.renameNodes (newId |> String.fromInt) copiedTree

        pid =
            (getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0"

        pos =
            (getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1
    in
    ( { model | globalData = GlobalData.setSeed newSeed model.globalData }
    , prevCmd
    )
        |> paste treeToPaste pid pos


pasteInto : String -> Tree -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteInto id copiedTree ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomPositiveInt (GlobalData.seed model.globalData)

        treeToPaste =
            TreeStructure.renameNodes (newId |> String.fromInt) copiedTree
    in
    ( { model | globalData = GlobalData.setSeed newSeed model.globalData }
    , prevCmd
    )
        |> paste treeToPaste id 999999



-- === Local Saving ===


localSave : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
localSave ( model, prevCmd ) =
    updateDoc (ThrottledLocalSave (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])



-- === History ===


checkoutCommit : String -> Model -> ( Model, Cmd Msg )
checkoutCommit commitSha model =
    let
        newTree_ =
            Data.checkout commitSha model.data
    in
    case newTree_ of
        Just newTree ->
            ( { model
                | workingTree = TreeStructure.setTree newTree model.workingTree
              }
            , Cmd.none
            )
                |> activate model.viewState.active False

        Nothing ->
            ( model
            , Cmd.none
            )



-- History


addToHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistory ( model, prevCmd ) =
    updateDoc (ThrottledCommit (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])



-- === Files ===


dataReceived : Json.Value -> Model -> ( Model, Cmd Msg )
dataReceived dataIn model =
    case Data.received dataIn ( model.data, model.workingTree.tree ) of
        Just { newModel, newTree } ->
            let
                vs =
                    model.viewState

                newWorkingTree =
                    TreeStructure.setTreeWithConflicts (Data.conflictList newModel) newTree model.workingTree

                startingWordcount =
                    countWords (treeToMarkdownString False newTree)

                ( newViewState, maybeScroll ) =
                    case Json.decodeValue (Json.at [ "localStore", "last-actives" ] (Json.list Json.string)) dataIn of
                        Ok (lastActive :: activePast) ->
                            ( { vs | active = lastActive, activePast = activePast }, activate lastActive True )

                        _ ->
                            ( vs, activate "1" True )
            in
            ( { model
                | data = newModel
                , loading = False
                , workingTree = newWorkingTree
                , viewState = newViewState
                , lastLocalSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
                , lastRemoteSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
                , startingWordcount = startingWordcount
              }
            , Cmd.none
            )
                |> maybeScroll

        Nothing ->
            ( model, Cmd.none )



-- VIEW


type alias AppMsgs msg =
    { docMsg : Msg -> msg
    , keyboard : String -> msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }


view : AppMsgs msg -> Model -> List (Html msg)
view appMsg model =
    if model.loading then
        UI.viewDocumentLoadingSpinner

    else
        viewLoaded appMsg model


viewLoaded : AppMsgs msg -> Model -> List (Html msg)
viewLoaded ({ docMsg } as appMsg) model =
    let
        language =
            GlobalData.language model.globalData

        isMac =
            GlobalData.isMac model.globalData
    in
    case Data.conflictList model.data of
        [] ->
            if model.viewState.viewMode == FullscreenEditing then
                lazy3 Fullscreen.view
                    { language = language
                    , isMac = isMac
                    , dirty = model.dirty
                    , model = model.workingTree
                    , lastLocalSave = model.lastLocalSave
                    , lastRemoteSave = model.lastRemoteSave
                    , currentTime = GlobalData.currentTime model.globalData
                    }
                    model.field
                    model.viewState
                    |> Html.map (docMsg << FullscreenMsg)
                    |> List.singleton

            else
                let
                    activeTree_ =
                        getTree model.viewState.active model.workingTree.tree

                    mobileBtnMsg shortcut =
                        appMsg.keyboard shortcut

                    cardTitleReplacer ( id, inputString ) =
                        case String.lines inputString of
                            firstLine :: _ ->
                                ( id
                                , firstLine
                                    |> String.trim
                                    |> (\str ->
                                            if String.isEmpty str then
                                                "(empty)"

                                            else
                                                str
                                       )
                                )

                            [] ->
                                ( id, "(empty)" )

                    cardTitles =
                        case activeTree_ of
                            Just activeTree ->
                                (getAncestors model.workingTree.tree activeTree []
                                    |> List.map (\t -> ( t.id, t.content ))
                                    |> List.drop 1
                                )
                                    ++ [ ( activeTree.id, activeTree.content ) ]
                                    |> List.map cardTitleReplacer

                            Nothing ->
                                []
                in
                [ lazy4 treeView (GlobalData.language model.globalData) (GlobalData.isMac model.globalData) model.viewState model.workingTree |> Html.map docMsg
                , if (not << List.isEmpty) cardTitles then
                    UI.viewBreadcrumbs Activate cardTitles |> Html.map docMsg

                  else
                    text ""
                ]
                    ++ [ viewSearchField SearchFieldUpdated model |> Html.map docMsg
                       , viewMobileButtons
                            { edit = mobileBtnMsg "mod+enter"
                            , save = mobileBtnMsg "mod+enter"
                            , cancel = mobileBtnMsg "esc"
                            , plusDown = mobileBtnMsg "mod+down"
                            , plusUp = mobileBtnMsg "mod+up"
                            , plusRight = mobileBtnMsg "mod+right"
                            , navLeft = mobileBtnMsg "left"
                            , navUp = mobileBtnMsg "up"
                            , navDown = mobileBtnMsg "down"
                            , navRight = mobileBtnMsg "right"
                            }
                            (model.viewState.viewMode /= Normal)
                       , Keyed.node "div" [ style "display" "contents" ] [ ( "randomstringforloadingoverlay", div [ id "loading-overlay" ] [] ) ]
                       , div [ id "preloader" ] []
                       ]

        conflicts ->
            let
                bgString =
                    """
repeating-linear-gradient(-45deg
, rgba(255,255,255,0.02)
, rgba(255,255,255,0.02) 15px
, rgba(0,0,0,0.025) 15px
, rgba(0,0,0,0.06) 30px
)
          """
            in
            [ ul [ class "conflicts-list" ]
                (List.map (viewConflict language SetSelection Resolve) conflicts)
                |> Html.map docMsg
            , lazy4 treeView (GlobalData.language model.globalData) (GlobalData.isMac model.globalData) model.viewState model.workingTree |> Html.map docMsg
            ]


treeView : Language -> Bool -> ViewState -> TreeStructure.Model -> Html Msg
treeView lang isMac vstate model =
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

        columnsFiltered =
            model.columns
                |> searchFilter vstate.searchField
                |> List.drop 1

        getViewArgs c =
            let
                editing_ =
                    case vstate.viewMode of
                        Normal ->
                            Normal

                        Editing ->
                            if c |> List.concat |> List.map .id |> List.member vstate.active then
                                Editing

                            else
                                Normal

                        FullscreenEditing ->
                            -- TODO : Impossible state
                            Normal
            in
            VisibleViewState
                vstate.active
                editing_
                vstate.descendants
                vstate.ancestors
                vstate.dragModel
                vstate.collaborators
                lang
                isMac

        columns =
            columnsFiltered
                |> List.map (\c -> lazy2 viewColumn (getViewArgs c) c)
    in
    div
        [ id "document"
        ]
        [ div [ class "left-padding-column" ] []
        , div [ id "column-container" ]
            columns
        , div [ class "right-padding-column" ] []
        ]


viewColumn : VisibleViewState -> Column -> Html Msg
viewColumn vstate col =
    let
        buffer =
            [ div [ class "buffer" ] [] ]
    in
    div
        [ class "column" ]
        (buffer
            ++ List.map (lazy2 viewGroup vstate) col
            ++ buffer
        )


viewGroup : VisibleViewState -> Group -> Html Msg
viewGroup vstate xs =
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
                    case vstate.viewMode of
                        Editing ->
                            t.id == vstate.active

                        Normal ->
                            False

                        FullscreenEditing ->
                            -- TODO : Impossible state
                            False

                isLast =
                    t.id == lastChild

                collabsEditingCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == CollabEditing t.id)
                        |> List.map .uid

                collabsOnCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == CollabActive t.id || c.mode == CollabEditing t.id)
                        |> List.map .uid
            in
            if isActive && not isEditing then
                ( t.id, lazy8 viewCardActive vstate.language t.id t.content (hasChildren t) isLast collabsOnCard collabsEditingCard vstate.dragModel )

            else if isEditing then
                ( t.id, viewCardEditing vstate.language t.id t.content (hasChildren t) vstate.isMac )

            else
                ( t.id, lazy7 viewCardOther t.id t.content isEditing (hasChildren t) isAncestor isLast vstate.dragModel )
    in
    Keyed.node "div"
        [ classList
            [ ( "group", True )
            , ( "has-active", hasActive )
            , ( "active-descendant", isActiveDescendant )
            ]
        ]
        (List.map viewFunction xs
            ++ (if isActiveDescendant then
                    [ ( "fillet-top-left", UI.fillet "top-left" )
                    , ( "fillet-bottom-left", UI.fillet "bottom-left" )
                    , ( "fillet-top-right", UI.fillet "top-right" )
                    , ( "fillet-bottom-right", UI.fillet "bottom-right" )
                    ]

                else
                    []
               )
        )


viewCardOther : String -> String -> Bool -> Bool -> Bool -> Bool -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardOther cardId content isEditing isParent isAncestor isLast dragModels =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "ancestor", isAncestor )
            , ( "has-children", isParent )
            ]
        ]
        ((if not isEditing then
            [ div ([ class "drag-region", title "Drag to move" ] ++ DragDrop.draggable DragDropMsg cardId) [ div [ class "handle" ] [] ] ]

          else
            []
         )
            ++ dropRegions cardId isEditing isLast dragModels
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               ]
        )


viewCardActive : Language -> String -> String -> Bool -> Bool -> List String -> List String -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardActive lang cardId content isParent isLast collabsOnCard collabsEditingCard dragModels =
    let
        buttons =
            [ div [ class "flex-row card-top-overlay" ]
                [ span
                    [ class "card-btn ins-above"
                    , title <| tr lang InsertAboveTitle
                    , onClick (InsertAbove cardId)
                    ]
                    [ text "+" ]
                ]
            , div [ class "flex-column card-right-overlay" ]
                [ span
                    [ class "card-btn delete"
                    , title <| tr lang DeleteCardTitle
                    , onClick (DeleteCard cardId)
                    ]
                    []
                , span
                    [ class "card-btn ins-right"
                    , title <| tr lang InsertChildTitle
                    , onClick (InsertChild cardId)
                    ]
                    [ text "+" ]
                , span
                    [ class "card-btn edit"
                    , title <| tr lang EditCardTitle
                    , onClick (OpenCard cardId content)
                    ]
                    []
                ]
            , div [ class "flex-row card-bottom-overlay" ]
                [ span
                    [ class "card-btn ins-below"
                    , title <| tr lang InsertBelowTitle
                    , onClick (InsertBelow cardId)
                    ]
                    [ text "+" ]
                ]
            , viewIf isParent <| UI.fillet "top-right"
            , viewIf isParent <| UI.fillet "bottom-right"
            ]
    in
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "collab-active", not (List.isEmpty collabsOnCard) )
            , ( "collab-editing", not (List.isEmpty collabsEditingCard) )
            , ( "has-children", isParent )
            ]
        ]
        ([ div ([ class "drag-region", title "Drag to move" ] ++ DragDrop.draggable DragDropMsg cardId) [ div [ class "handle" ] [] ] ]
            ++ buttons
            ++ dropRegions cardId False isLast dragModels
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardEditing : Language -> String -> String -> Bool -> Bool -> Html Msg
viewCardEditing lang cardId content isParent isMac =
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "active", True )
            , ( "editing", True )
            , ( "has-children", isParent )
            ]
        , attribute "data-cloned-content" content
        ]
        [ textarea
            [ id ("card-edit-" ++ cardId)
            , dir "auto"
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , attribute "data-private" "lipsum"
            , onInput <| UpdateActiveField cardId
            , value content
            ]
            []
        , div [ class "flex-column card-right-overlay" ]
            [ div
                [ class "fullscreen-card-btn"
                , title "Edit in Fullscreen"
                , onClick EditToFullscreenMode
                ]
                [ AntIcons.fullscreenOutlined [ Attributes.width 16, Attributes.height 16 ] ]
            , div
                [ class "card-btn save"
                , title <| tr lang SaveChangesTitle
                , onClick SaveAndCloseCard
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


dropRegions : String -> Bool -> Bool -> ( DragDrop.Model String DropId, DragExternalModel ) -> List (Html Msg)
dropRegions cardId isEditing isLast ( dragModel, dragExternalModel ) =
    let
        dragId_ =
            DragDrop.getDragId dragModel

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

        dropDivExternal str dId =
            div
                [ classList
                    [ ( "drop-region-" ++ str, True )
                    , ( "drop-hover", .dropId dragExternalModel == Just dId )
                    ]
                , onWithOptions "dragenter" { stopPropagation = True, preventDefault = True } <| Json.succeed <| DragExternal <| DragEnter dId
                , onWithOptions "dragleave" { stopPropagation = True, preventDefault = True } <| Json.succeed <| DragExternal <| DragLeave dId
                ]
                []
    in
    case ( dragId_, dragExternalModel.isDragging, isEditing ) of
        ( Just _, _, False ) ->
            [ dropDiv "above" (Above cardId)
            , dropDiv "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDiv "below" (Below cardId) ]

                    else
                        []
                   )

        ( Nothing, True, False ) ->
            [ dropDivExternal "above" (Above cardId)
            , dropDivExternal "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDivExternal "below" (Below cardId) ]

                    else
                        []
                   )

        _ ->
            []


viewContent : String -> String -> Html Msg
viewContent cardId content =
    let
        options =
            { githubFlavored = Just { tables = True, breaks = True }
            , defaultHighlighting = Nothing
            , sanitize = False
            , smartypants = False
            }

        processedContent =
            let
                checkboxes =
                    Regex.fromStringWith { caseInsensitive = True, multiline = True }
                        "\\[(x| )\\]"
                        |> Maybe.withDefault Regex.never

                openAddDiff =
                    Regex.fromString "{\\+\\+" |> Maybe.withDefault Regex.never

                closeAddDiff =
                    Regex.fromString "\\+\\+}" |> Maybe.withDefault Regex.never

                openDelDiff =
                    Regex.fromString "{--" |> Maybe.withDefault Regex.never

                closeDelDiff =
                    Regex.fromString "--}" |> Maybe.withDefault Regex.never

                checkboxReplacer { match, number } =
                    let
                        checkState =
                            if match == "[x]" || match == "[X]" then
                                "checked"

                            else
                                ""
                    in
                    "<input type='checkbox'"
                        ++ checkState
                        ++ " onClick='checkboxClicked(\""
                        ++ cardId
                        ++ "\", "
                        ++ String.fromInt number
                        ++ ")'/>"
            in
            content
                |> Regex.replace openAddDiff (\_ -> "<ins class='diff'>")
                |> Regex.replace closeAddDiff (\_ -> "</ins>")
                |> Regex.replace openDelDiff (\_ -> "<del class='diff'>")
                |> Regex.replace closeDelDiff (\_ -> "</del>")
                |> Regex.replace checkboxes checkboxReplacer
    in
    Markdown.toHtmlWith options
        [ attribute "data-private" "lipsum" ]
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.dirty then
            Time.every (241 * 1000) (always AutoSave)

          else
            Sub.none
        , Time.every (23 * 1000) (always Pull)
        ]



-- HELPERS


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus ("card-edit-" ++ id))


normalMode : Model -> (( Model, Cmd Msg ) -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
normalMode model operation =
    ( model
    , Cmd.none
    )
        |> (case model.viewState.viewMode of
                Normal ->
                    operation

                _ ->
                    identity
           )


onWithOptions :
    String
    ->
        { stopPropagation : Bool
        , preventDefault : Bool
        }
    -> Json.Decoder msg
    -> Attribute msg
onWithOptions name { stopPropagation, preventDefault } decoder =
    decoder
        |> Json.map (\msg -> { message = msg, stopPropagation = stopPropagation, preventDefault = preventDefault })
        |> custom name
