module Page.Doc exposing (Model, Msg, MsgToParent(..), getActiveId, getActiveTree, getCollaborators, getGlobalData, getTextCursorInfo, getViewMode, getWorkingTree, init, isDirty, isFullscreen, isNormalMode, lastActives, maybeActivate, opaqueIncoming, opaqueUpdate, setBlock, setDirty, setGlobalData, setLoading, setTree, setWorkingTree, subscriptions, toggleEditing, view)

import Ant.Icons.Svg as AntIcons
import Browser.Dom exposing (Element)
import Coders exposing (collabStateEncoder, treeToValue)
import Doc.Fonts as Fonts
import Doc.Fullscreen as Fullscreen
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils exposing (..)
import Doc.UI as UI exposing (viewMobileButtons, viewSearchField)
import GlobalData exposing (GlobalData)
import Html exposing (Attribute, Html, div, node, span, text)
import Html.Attributes as Attributes exposing (attribute, class, classList, dir, id, style, title)
import Html.Events exposing (custom, onClick, onDoubleClick)
import Html.Extra exposing (viewIf)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy4, lazy5, lazy7, lazy8)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import List.Extra as ListExtra
import Markdown
import Outgoing exposing (Msg(..), send)
import Page.Doc.Incoming as Incoming exposing (Msg(..))
import Random
import RandomId exposing (stringGenerator)
import Regex
import Task
import Time
import Translation exposing (Language, TranslationId(..), tr)
import Types exposing (..)
import UI.Collaborators



-- MODEL


type alias ModelData =
    -- Document state
    { workingTree : TreeStructure.Model

    -- SPA Page State
    , globalData : GlobalData
    , loading : Bool
    , block : Maybe String

    -- Transient state
    , viewState : ViewState
    , dirty : Bool
    , textCursorInfo : TextCursorInfo
    , fileSearchField : String

    -- Settings
    , uid : String
    , fonts : Fonts.Model
    }


type Model
    = Model ModelData


init : Bool -> GlobalData -> Model
init isNew globalData =
    Model
        { workingTree = TreeStructure.defaultModel
        , globalData = globalData
        , loading = not isNew
        , block = Nothing
        , uid = "0"
        , viewState =
            { viewMode =
                if isNew then
                    Editing { cardId = "1", field = "" }

                else
                    Normal ""
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
        , textCursorInfo = { selected = False, position = End, text = ( "", "" ) }
        , fileSearchField = ""
        , fonts = Fonts.default
        }



-- UPDATE


type Msg
    = NoOp
      -- === Card Activation ===
    | Activate String
    | SearchFieldUpdated String
      -- === Card Editing  ===
    | OpenCard String String
    | AutoSave
    | SaveAndCloseCard
    | EditToFullscreenMode
    | DeleteCard String
      -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
      -- === Dragging ===
    | DragDropMsg (DragDrop.Msg String DropId)
    | DragExternal DragExternalMsg
      -- === Fullscreen ===
    | ExitFullscreenRequested
    | SaveFromFullscreen
    | SaveAndExitFullscreen
      -- === Ports ===
    | LogErr String


type MsgToParent
    = ParentAddToast ToastPersistence Toast
    | CloseTooltip
    | LocalSave CardTreeOp
    | Commit


type DragExternalMsg
    = DragEnter DropId
    | DragLeave DropId


opaqueUpdate : Msg -> Model -> ( Model, Cmd Msg, List MsgToParent )
opaqueUpdate msg (Model model) =
    let
        ( newModel, cmd, pMsgs ) =
            update msg model
    in
    ( Model newModel, cmd, pMsgs )


update : Msg -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
update msg ({ workingTree } as model) =
    let
        vs =
            model.viewState
    in
    case msg of
        -- === Card Activation ===
        Activate id ->
            changeMode { to = Normal id, instant = False, save = True } model

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
                            ( \( m, c, p ) ->
                                ( m
                                , Cmd.batch [ c, Task.attempt (\_ -> NoOp) (Browser.Dom.blur "search-input") ]
                                , p
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

                maybeChangeActive =
                    case ( newSearchField, firstFilteredCardId_ ) of
                        ( Just _, Just id ) ->
                            activate id False

                        ( Nothing, _ ) ->
                            activate (getActiveId (Model model)) False

                        _ ->
                            identity
            in
            ( { model | viewState = { vs | searchField = newSearchField } }
            , Cmd.none
            , []
            )
                |> maybeBlur
                |> maybeChangeActive

        -- === Card Editing  ===
        OpenCard id str ->
            model
                |> openCard id str

        AutoSave ->
            ( model, Cmd.none, [] ) |> saveCardIfEditing

        SaveAndCloseCard ->
            changeMode
                { to = Normal (getActiveId (Model model)), instant = True, save = True }
                model

        EditToFullscreenMode ->
            case vs.viewMode of
                Editing { cardId, field } ->
                    changeMode
                        { to = FullscreenEditing { cardId = cardId, field = field }
                        , instant = True
                        , save = True
                        }
                        model

                _ ->
                    ( model, Cmd.none, [] )

        DeleteCard id ->
            ( model
            , Cmd.none
            , []
            )
                |> deleteCard id

        -- === Card Insertion  ===
        InsertAbove id ->
            ( model
            , Cmd.none
            , []
            )
                |> insertAbove id ""

        InsertBelow id ->
            ( model
            , Cmd.none
            , []
            )
                |> insertBelow id ""

        InsertChild id ->
            ( model
            , Cmd.none
            , []
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
                    , []
                    )
                        |> preventIfBlocked model

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
                            ( { modelDragUpdated | viewState = { vs | draggedTree = Nothing }, dirty = True }, Cmd.batch [ send <| SetDirty True, send <| DragDone ], [] )
                                |> moveOperation
                                |> andThen (changeMode { to = Normal draggedTree.id, instant = False, save = True })

                        Nothing ->
                            ( modelDragUpdated, Cmd.none, [] )

                ( Nothing, Nothing ) ->
                    -- NotDragging
                    case vs.draggedTree of
                        Just ( draggedTree, parentId, idx ) ->
                            ( modelDragUpdated, Cmd.none, [] )
                                |> move draggedTree parentId idx

                        Nothing ->
                            ( modelDragUpdated, Cmd.none, [] )

                ( Just _, Just _ ) ->
                    -- Should be Impossible: both Dragging and Dropped
                    ( modelDragUpdated, Cmd.none, [] )

        DragExternal dragExternalMsg ->
            case dragExternalMsg of
                DragEnter dId ->
                    ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Just dId, isDragging = True } ) } }, Cmd.none, [] )

                DragLeave dId ->
                    if (Tuple.second vs.dragModel |> .dropId) == Just dId then
                        ( { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = True } ) } }, Cmd.none, [] )

                    else
                        ( model, Cmd.none, [] )

        -- === Fullscreen ===
        ExitFullscreenRequested ->
            case vs.viewMode of
                FullscreenEditing { cardId, field } ->
                    changeMode
                        { to = Editing { cardId = cardId, field = field }
                        , instant = True
                        , save = True
                        }
                        model

                _ ->
                    ( model, Cmd.none, [] )

        SaveFromFullscreen ->
            case vs.viewMode of
                FullscreenEditing { cardId, field } ->
                    saveCard { cardId = cardId, field = field } model

                _ ->
                    ( model, Cmd.none, [] )

        SaveAndExitFullscreen ->
            case vs.viewMode of
                FullscreenEditing { cardId, field } ->
                    changeMode
                        { to = Normal cardId
                        , instant = True
                        , save = True
                        }
                        model

                _ ->
                    ( model, Cmd.none, [] )

        -- === Ports ===
        LogErr _ ->
            ( model, Cmd.none, [] )

        NoOp ->
            ( model
            , Cmd.none
            , []
            )


localSave : CardTreeOp -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
localSave op ( model, cmd, prevMsgsToParent ) =
    ( model
    , cmd
    , prevMsgsToParent ++ [ LocalSave op ]
    )


addToHistory : ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
addToHistory ( model, cmd, prevMsgsToParent ) =
    ( model
    , cmd
    , prevMsgsToParent ++ [ Commit ]
    )


opaqueIncoming : Incoming.Msg -> Model -> ( Model, Cmd Msg, List MsgToParent )
opaqueIncoming msg (Model model) =
    let
        ( newModel, cmd, msgsToParent ) =
            incoming msg model
    in
    ( Model newModel, cmd, msgsToParent )


incoming : Incoming.Msg -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
incoming incomingMsg model =
    let
        vs =
            model.viewState

        activeId =
            getActiveId (Model model)
    in
    case incomingMsg of
        -- === Dialogs, Menus, Window State ===
        CancelCardConfirmed ->
            ( { model | dirty = False }
            , send <| SetDirty False
            , []
            )
                |> cancelCard

        -- === DOM ===
        InitialActivation firstCardId ->
            case vs.viewMode of
                Normal currId ->
                    ( model, Cmd.none, [] )
                        |> andThen (changeMode { to = Normal activeId, instant = True, save = False })

                Editing _ ->
                    ( model, Cmd.none, [] )
                        |> andThen
                            (changeMode
                                { to = Editing { cardId = firstCardId, field = "" }
                                , instant = True
                                , save = False
                                }
                            )

                _ ->
                    ( model, Cmd.none, [] )

        DragStarted dragId ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Rmv dragId) model.workingTree

                draggedTree =
                    getTreeWithPosition dragId model.workingTree.tree
            in
            if List.isEmpty <| getChildren newTree.tree then
                ( model, Cmd.none, [] )

            else
                ( { model | workingTree = newTree, viewState = { vs | draggedTree = draggedTree } }, Cmd.none, [] )

        DragExternalStarted ->
            case vs.viewMode of
                Normal _ ->
                    ( { model
                        | viewState =
                            { vs
                                | dragModel =
                                    ( Tuple.first vs.dragModel
                                    , { dropId = Nothing, isDragging = True }
                                    )
                            }
                      }
                    , Cmd.none
                    , []
                    )

                _ ->
                    ( model, Cmd.none, [] )

        DropExternal dropText ->
            case Tuple.second vs.dragModel |> .dropId of
                Just dropId ->
                    let
                        modelNoDrag =
                            { model | viewState = { vs | dragModel = ( Tuple.first vs.dragModel, { dropId = Nothing, isDragging = False } ) } }

                        baseModelCmdTuple =
                            case dropId of
                                Above cardId ->
                                    ( modelNoDrag, Cmd.none, [] ) |> insertAbove cardId dropText

                                Into cardId ->
                                    ( modelNoDrag, Cmd.none, [] ) |> insertChild cardId dropText

                                Below cardId ->
                                    ( modelNoDrag, Cmd.none, [] ) |> insertBelow cardId dropText
                    in
                    baseModelCmdTuple
                        |> closeCard
                        |> addToHistory

                Nothing ->
                    ( model, Cmd.none, [] )

        Paste tree ->
            normalMode model (pasteBelow activeId tree)

        PasteInto tree ->
            normalMode model (pasteInto activeId tree)

        FieldChanged str ->
            case vs.viewMode of
                Editing { cardId } ->
                    ( { model
                        | viewState = { vs | viewMode = Editing { cardId = cardId, field = str } }
                        , dirty = True
                      }
                    , send <| SetDirty True
                    , []
                    )

                FullscreenEditing { cardId } ->
                    ( { model
                        | viewState = { vs | viewMode = FullscreenEditing { cardId = cardId, field = str } }
                        , dirty = True
                      }
                    , send <| SetDirty True
                    , []
                    )

                _ ->
                    ( model, Cmd.none, [] )

        AutoSaveRequested ->
            ( model, Cmd.none, [] )
                |> saveCardIfEditing

        FullscreenCardFocused newCardId newCardContent ->
            case vs.viewMode of
                FullscreenEditing { cardId } ->
                    if cardId /= newCardId then
                        changeMode
                            { to = FullscreenEditing { cardId = newCardId, field = newCardContent }
                            , instant = False
                            , save = True
                            }
                            model

                    else
                        ( model, Cmd.none, [] )

                _ ->
                    ( model, Cmd.none, [] )

        TextCursor textCursorInfo ->
            if model.textCursorInfo /= textCursorInfo then
                ( { model | textCursorInfo = textCursorInfo }
                , Cmd.none
                , []
                )

            else
                ( model, Cmd.none, [] )

        ClickedOutsideCard ->
            case vs.viewMode of
                FullscreenEditing _ ->
                    ( model, Cmd.none, [] )

                _ ->
                    changeMode { to = Normal (getActiveIdFromViewState vs), instant = False, save = True } model

        CheckboxClicked cardId checkboxNumber ->
            case getTree cardId model.workingTree.tree of
                Nothing ->
                    ( model, Cmd.none, [] )

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
                    ( { model | workingTree = newTree, dirty = True }, Cmd.none, [] )
                        |> localSave (CTUpd cardId newContent)
                        |> addToHistory

        -- === UI ===
        Keyboard shortcut ->
            case shortcut of
                "shift+enter" ->
                    case vs.viewMode of
                        Normal active ->
                            changeMode
                                { to =
                                    FullscreenEditing
                                        { cardId = active
                                        , field = getContent active model.workingTree.tree
                                        }
                                , instant = False
                                , save = False
                                }
                                model

                        _ ->
                            ( model, Cmd.none, [] )

                "mod+enter" ->
                    toggleEditing model

                "mod+s" ->
                    saveCardIfEditing ( model, Cmd.none, [] )

                "enter" ->
                    normalMode model (andThen <| openCard activeId (getContent activeId model.workingTree.tree))

                "mod+backspace" ->
                    normalMode model (deleteCard activeId)

                "esc" ->
                    model |> intentCancelCard

                "mod+j" ->
                    case vs.viewMode of
                        Normal active ->
                            insertBelow active "" ( model, Cmd.none, [] )

                        Editing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( model, Cmd.none, [] )
                                |> andThen (saveCard { cardId = cardId, field = beforeText })
                                |> insertBelow activeId afterText
                                |> setCursorPosition 0

                        FullscreenEditing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( model, Cmd.none, [] )
                                |> andThen (saveCard { cardId = cardId, field = beforeText })
                                |> insertBelow activeId afterText
                                |> setCursorPosition 0

                "mod+down" ->
                    normalMode model (insertBelow activeId "")

                "mod+k" ->
                    case vs.viewMode of
                        Normal active ->
                            insertAbove active "" ( model, Cmd.none, [] )

                        Editing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( { model
                                | viewState =
                                    { vs | viewMode = Editing { cardId = cardId, field = afterText } }
                              }
                            , Cmd.none
                            , []
                            )
                                |> saveCardIfEditing
                                |> insertAbove activeId beforeText

                        FullscreenEditing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( { model
                                | viewState =
                                    { vs | viewMode = FullscreenEditing { cardId = cardId, field = afterText } }
                              }
                            , Cmd.none
                            , []
                            )
                                |> saveCardIfEditing
                                |> insertAbove activeId beforeText

                "mod+up" ->
                    normalMode model (insertAbove activeId "")

                "mod+l" ->
                    case vs.viewMode of
                        Normal active ->
                            insertChild active "" ( model, Cmd.none, [] )

                        Editing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( { model
                                | viewState =
                                    { vs | viewMode = Editing { cardId = cardId, field = beforeText } }
                              }
                            , Cmd.none
                            , []
                            )
                                |> saveCardIfEditing
                                |> insertChild activeId afterText
                                |> setCursorPosition 0

                        FullscreenEditing { cardId } ->
                            let
                                ( beforeText, afterText ) =
                                    model.textCursorInfo.text
                            in
                            ( { model
                                | viewState =
                                    { vs | viewMode = FullscreenEditing { cardId = cardId, field = beforeText } }
                              }
                            , Cmd.none
                            , []
                            )
                                |> saveCardIfEditing
                                |> insertChild activeId afterText
                                |> setCursorPosition 0

                "mod+right" ->
                    normalMode model (insertChild activeId "")

                "mod+shift+j" ->
                    normalMode model (mergeDown activeId)

                "mod+shift+down" ->
                    normalMode model (mergeDown activeId)

                "mod+shift+k" ->
                    normalMode model (mergeUp activeId)

                "mod+shift+up" ->
                    normalMode model (mergeUp activeId)

                "h" ->
                    normalMode model (goLeft activeId)

                "left" ->
                    normalMode model (goLeft activeId)

                "j" ->
                    normalMode model (goDown activeId)

                "down" ->
                    case vs.viewMode of
                        Normal active ->
                            ( model, Cmd.none, [] )
                                |> goDown active

                        FullscreenEditing _ ->
                            {- check if at end
                               if so, getNextInColumn and openCardFullscreen it
                            -}
                            ( model, Cmd.none, [] )

                        Editing _ ->
                            ( model, Cmd.none, [] )

                "k" ->
                    normalMode model (goUp activeId)

                "up" ->
                    normalMode model (goUp activeId)

                "l" ->
                    normalMode model (goRight activeId)

                "right" ->
                    normalMode model (goRight activeId)

                "alt+up" ->
                    normalMode model (moveWithin activeId -1)

                "alt+k" ->
                    normalMode model (moveWithin activeId -1)

                "alt+down" ->
                    normalMode model (moveWithin activeId 1)

                "alt+j" ->
                    normalMode model (moveWithin activeId 1)

                "alt+left" ->
                    normalMode model (moveLeft activeId)

                "alt+h" ->
                    normalMode model (moveLeft activeId)

                "alt+right" ->
                    normalMode model (moveRight activeId)

                "alt+l" ->
                    normalMode model (moveRight activeId)

                "alt+shift+up" ->
                    normalMode model (moveWithin activeId -5)

                "alt+shift+down" ->
                    normalMode model (moveWithin activeId 5)

                "alt+pageup" ->
                    normalMode model (moveWithin activeId -999999)

                "alt+pagedown" ->
                    normalMode model (moveWithin activeId 999999)

                "home" ->
                    normalMode model (goToTopOfColumn activeId)

                "end" ->
                    normalMode model (goToBottomOfColumn activeId)

                "pageup" ->
                    normalMode model (goToTopOfGroup activeId True)

                "pagedown" ->
                    normalMode model (goToBottomOfGroup activeId True)

                "mod+x" ->
                    normalMode model (cut activeId)

                "mod+c" ->
                    normalMode model (copy activeId)

                "mod+b" ->
                    case vs.viewMode of
                        Normal _ ->
                            ( model
                            , Cmd.none
                            , []
                            )

                        _ ->
                            ( model
                            , send (TextSurround activeId "**")
                            , []
                            )

                "mod+i" ->
                    case vs.viewMode of
                        Normal _ ->
                            ( model
                            , Cmd.none
                            , []
                            )

                        _ ->
                            ( model
                            , send (TextSurround activeId "*")
                            , []
                            )

                "/" ->
                    case vs.viewMode of
                        Normal _ ->
                            ( model
                            , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-input")
                            , []
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            , []
                            )

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        WillPrint ->
            ( model, Cmd.none, [] )

        -- === Misc ===
        RecvCollabState collabState ->
            let
                assignedInts =
                    List.map .int vs.collaborators

                nextAvailableInt =
                    List.range 0 (List.length vs.collaborators + 1)
                        |> List.filter (\i -> not (List.member i assignedInts))
                        |> List.head
                        |> Maybe.withDefault 0
                        |> Debug.log "nextAvailableInt"

                newCollabs =
                    if List.member collabState.uid (vs.collaborators |> List.map .uid) then
                        vs.collaborators
                            |> List.map
                                (\c ->
                                    if c.uid == collabState.uid then
                                        { collabState | int = c.int }

                                    else
                                        c
                                )

                    else
                        { collabState | int = nextAvailableInt } :: vs.collaborators
            in
            ( { model
                | viewState = { vs | collaborators = newCollabs |> Debug.log "newCollabs" }
              }
            , Cmd.none
            , []
            )

        RecvCollabUsers users ->
            ( { model
                | viewState =
                    { vs
                        | collaborators =
                            users
                                |> List.sortBy .uid
                                |> List.indexedMap (\i u -> { u | int = i })
                    }
              }
            , Cmd.none
            , []
            )

        CollaboratorDisconnected uid ->
            ( { model
                | viewState =
                    { vs | collaborators = vs.collaborators |> List.filter (\c -> c.uid /= uid) }
              }
            , Cmd.none
            , []
            )

        -- === INTEGRATION TEST HOOKS ===
        TestTextImportLoaded _ ->
            ( model, Cmd.none, [] )


andThen : (ModelData -> ( ModelData, Cmd Msg, List MsgToParent )) -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
andThen f ( model, cmd, msgs ) =
    let
        ( newModel, nextCmd, newMsgs ) =
            f model
    in
    ( newModel, Cmd.batch [ cmd, nextCmd ], msgs ++ newMsgs )



-- === Card Activation ===


changeMode : { to : ViewMode, instant : Bool, save : Bool } -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
changeMode { to, instant, save } model =
    let
        vs =
            model.viewState

        oldId =
            getActiveIdFromViewMode vs.viewMode

        tryId =
            getActiveIdFromViewMode to

        targetTree_ =
            getTree (getActiveIdFromViewMode to) model.workingTree.tree
    in
    case targetTree_ of
        Just targetTree ->
            let
                newPast =
                    if tryId == oldId then
                        vs.activePast

                    else
                        oldId :: vs.activePast |> List.take 40

                id =
                    targetTree.id

                desc =
                    targetTree
                        |> getDescendants
                        |> List.map .id

                anc =
                    getAncestors model.workingTree.tree targetTree []
                        |> List.map .id

                scrollPositions =
                    getScrollPositions targetTree newPast model.workingTree.tree

                colIdx =
                    getDepth 0 model.workingTree.tree targetTree.id

                newModel newVm =
                    { model
                        | viewState =
                            { vs
                                | viewMode = newVm
                                , activePast = newPast
                                , descendants = desc
                                , ancestors = anc
                            }
                    }

                saveIfAsked eD =
                    if save then
                        andThen (saveCard eD)

                    else
                        identity

                scrollCmd =
                    send (ScrollCards (id :: newPast) scrollPositions colIdx instant)

                updateCollabState : Bool -> CollabStateMode -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
                updateCollabState condition newCollabState prevModel =
                    ( prevModel
                    , if instant || condition then
                        send (SendCollabState (collabStateEncoder newCollabState))

                      else
                        Cmd.none
                    , []
                    )
            in
            case ( vs.viewMode, to ) of
                ( Normal prevId, Normal newId ) ->
                    ( newModel to
                    , scrollCmd
                    , []
                    )
                        |> andThen
                            (updateCollabState (prevId /= newId)
                                (CollabActive newId)
                            )

                ( Editing oldEditData, Normal newId ) ->
                    ( newModel to, scrollCmd, [] )
                        |> saveIfAsked oldEditData
                        |> andThen
                            (updateCollabState True
                                (CollabActive newId)
                            )

                ( FullscreenEditing oldEditData, Normal newId ) ->
                    ( newModel to, scrollCmd, [] )
                        |> saveIfAsked oldEditData
                        |> andThen
                            (updateCollabState True
                                (CollabActive newId)
                            )

                ( Normal _, Editing newEditData ) ->
                    ( newModel to
                    , Cmd.batch [ focus newEditData.cardId, scrollCmd ]
                    , []
                    )
                        |> preventIfBlocked model
                        |> andThen
                            (updateCollabState True
                                (CollabEditing newEditData.cardId)
                            )

                ( Editing oldEditData, Editing newEditData ) ->
                    ( newModel to
                    , Cmd.batch [ focus newEditData.cardId, scrollCmd ]
                    , []
                    )
                        |> preventIfBlocked model
                        |> andThen
                            (updateCollabState (oldEditData.cardId /= newEditData.cardId)
                                (CollabEditing newEditData.cardId)
                            )

                ( FullscreenEditing oldEditData, Editing newEditData ) ->
                    ( newModel to
                    , Cmd.batch [ focus newEditData.cardId, scrollCmd ]
                    , []
                    )
                        |> saveIfAsked oldEditData
                        |> andThen
                            (updateCollabState (oldEditData.cardId /= newEditData.cardId)
                                (CollabEditing newEditData.cardId)
                            )

                ( Normal _, FullscreenEditing newEditData ) ->
                    ( newModel to, focus newEditData.cardId, [] )
                        |> andThen
                            (updateCollabState True
                                (CollabEditing newEditData.cardId)
                            )

                ( Editing oldEditData, FullscreenEditing newEditData ) ->
                    ( newModel to, focus newEditData.cardId, [] )
                        |> saveIfAsked oldEditData
                        |> andThen
                            (updateCollabState (oldEditData.cardId /= newEditData.cardId)
                                (CollabEditing newEditData.cardId)
                            )

                ( FullscreenEditing oldEditData, FullscreenEditing newEditData ) ->
                    ( newModel to, focus newEditData.cardId, [] )
                        |> saveIfAsked oldEditData
                        |> andThen
                            (updateCollabState (oldEditData.cardId /= newEditData.cardId)
                                (CollabEditing newEditData.cardId)
                            )

        Nothing ->
            case getFirstCard model.workingTree.tree of
                Just backupTree ->
                    changeMode { to = Normal backupTree.id, instant = instant, save = False } model

                Nothing ->
                    ( model, Cmd.none, [] )


activate : String -> Bool -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
activate tryId instant ( model, prevCmd, prevMsgsToParent ) =
    changeMode { to = Normal tryId, instant = instant, save = False } model
        |> andThen (\m -> ( m, prevCmd, prevMsgsToParent ))


goLeft : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goLeft id ( model, prevCmd, prevMsgsToParent ) =
    let
        targetId =
            getParent id model.workingTree.tree
                |> Maybe.withDefault defaultTree
                |> .id
                |> (\pId ->
                        if pId == "0" then
                            id

                        else
                            pId
                   )
    in
    ( model
    , prevCmd
    , prevMsgsToParent
    )
        |> andThen (changeMode { to = Normal targetId, instant = False, save = False })


goDown : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goDown id ( model, prevCmd, prevMsgsToParent ) =
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
    , prevMsgsToParent
    )
        |> andThen (changeMode { to = Normal targetId, instant = False, save = False })


goUp : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goUp id ( model, prevCmd, prevMsgsToParent ) =
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
    , prevMsgsToParent
    )
        |> andThen (changeMode { to = Normal targetId, instant = False, save = False })


goRight : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goRight id ( model, prevCmd, prevMsgsToParent ) =
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
            , prevMsgsToParent
            )

        Just _ ->
            if List.length childrenIds == 0 then
                ( model
                , prevCmd
                , prevMsgsToParent
                )

            else
                ( model
                , prevCmd
                , prevMsgsToParent
                )
                    |> andThen
                        (changeMode
                            { to = Normal prevActiveOfChildren
                            , instant = False
                            , save = False
                            }
                        )



-- === Card Editing  ===


saveCard : { cardId : String, field : String } -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
saveCard { cardId, field } model =
    let
        newTree =
            TreeStructure.update (TreeStructure.Upd cardId field) model.workingTree
    in
    if newTree.tree /= model.workingTree.tree then
        ( { model
            | workingTree = newTree
          }
        , Cmd.none
        , []
        )
            |> localSave (CTUpd cardId field)
            |> addToHistory

    else
        ( { model | dirty = False }
        , send <| SetDirty False
        , []
        )


toggleEditing : ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
toggleEditing model =
    let
        vs =
            model.viewState
    in
    case vs.viewMode of
        Normal active ->
            let
                isLocked =
                    vs.collaborators
                        |> List.filter (\c -> c.mode == CollabEditing active)
                        |> (not << List.isEmpty)
            in
            if isLocked then
                ( model
                , Cmd.none
                , ParentAddToast Temporary (Toast Warning "Card is being edited by someone else.") :: []
                )
                    |> preventIfBlocked model

            else
                changeMode
                    { to = Editing { cardId = active, field = getContent active model.workingTree.tree }
                    , instant = True
                    , save = True
                    }
                    model

        Editing { cardId, field } ->
            changeMode
                { to = Normal cardId
                , instant = True
                , save = True
                }
                model

        FullscreenEditing { cardId, field } ->
            changeMode
                { to = Normal cardId
                , instant = True
                , save = True
                }
                model


saveCardIfEditing : ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
saveCardIfEditing ( model, prevCmd, prevParentMsgs ) =
    let
        vs =
            model.viewState

        activeId =
            getActiveId (Model model)
    in
    case vs.viewMode of
        Normal _ ->
            ( model
            , prevCmd
            , prevParentMsgs
            )

        Editing { field } ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Upd activeId field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                  }
                , prevCmd
                , prevParentMsgs
                )
                    |> localSave (CTUpd activeId field)
                    |> addToHistory

            else
                ( { model | dirty = False }
                , Cmd.batch [ prevCmd, send <| SetDirty False ]
                , prevParentMsgs
                )

        FullscreenEditing { field } ->
            let
                newTree =
                    TreeStructure.update (TreeStructure.Upd activeId field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                  }
                , prevCmd
                , prevParentMsgs
                )
                    |> localSave (CTUpd activeId field)
                    |> addToHistory

            else
                ( { model | dirty = False }
                , Cmd.batch [ prevCmd, send <| SetDirty False ]
                , prevParentMsgs
                )


openCard : String -> String -> ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
openCard id str model =
    let
        isLocked =
            model.viewState.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)
    in
    if isLocked then
        ( model
        , Cmd.none
        , ParentAddToast Temporary (Toast Warning "Card is being edited by someone else.") :: []
        )
            |> preventIfBlocked model

    else
        changeMode { to = Editing { cardId = id, field = str }, instant = False, save = False } model


closeCard : ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
closeCard ( model, prevCmd, prevMsgsToParent ) =
    let
        vs =
            model.viewState

        activeId =
            getActiveId (Model model)
    in
    ( { model | viewState = { vs | viewMode = Normal activeId } }, prevCmd, prevMsgsToParent )


deleteCard : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
deleteCard id ( model, prevCmd, prevMsgsToParent ) =
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
        , prevCmd
        , ParentAddToast Temporary (Toast Warning "Card is being edited by someone else.") :: prevMsgsToParent
        )
            |> preventIfBlocked model

    else if isLastChild then
        ( model
        , prevCmd
        , ParentAddToast Temporary (Toast Warning "Cannot delete last card.") :: prevMsgsToParent
        )
            |> preventIfBlocked model

    else
        ( { model
            | workingTree = TreeStructure.update (TreeStructure.Rmv id) model.workingTree
            , dirty = True
          }
        , Cmd.batch [ prevCmd, send <| SetDirty True ]
        , prevMsgsToParent
        )
            |> localSave (CTRmv id)
            |> addToHistory
            |> preventIfBlocked model
            |> andThen (changeMode { to = Normal nextToActivate, instant = False, save = False })


goToTopOfColumn : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goToTopOfColumn id ( model, prevCmd, prevMsgsToParent ) =
    ( model
    , prevCmd
    , prevMsgsToParent
    )
        |> activate (getFirstInColumn id model.workingTree.tree) False


goToBottomOfColumn : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goToBottomOfColumn id ( model, prevCmd, prevMsgsToParent ) =
    ( model
    , prevCmd
    , prevMsgsToParent
    )
        |> activate (getLastInColumn id model.workingTree.tree) False


goToTopOfGroup : String -> Bool -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goToTopOfGroup id fallToNextGroup ( model, prevCmd, prevMsgsToParent ) =
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
    , prevMsgsToParent
    )
        |> activate targetId False


goToBottomOfGroup : String -> Bool -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
goToBottomOfGroup id fallToNextGroup ( model, prevCmd, prevMsgsToParent ) =
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
    , prevMsgsToParent
    )
        |> activate targetId False


cancelCard : ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
cancelCard ( model, prevCmd, prevMsgsToParent ) =
    let
        activeId =
            getActiveId (Model model)
    in
    ( model, prevCmd, prevMsgsToParent )
        |> andThen (changeMode { to = Normal activeId, instant = True, save = False })


intentCancelCard : ModelData -> ( ModelData, Cmd Msg, List MsgToParent )
intentCancelCard model =
    let
        vs =
            model.viewState

        activeId =
            getActiveId (Model model)

        originalContent =
            getContent activeId model.workingTree.tree
    in
    case vs.viewMode of
        Normal _ ->
            ( model
            , Cmd.none
            , []
            )

        _ ->
            ( model
            , send (ConfirmCancelCard activeId originalContent (tr (GlobalData.language model.globalData) AreYouSureCancel))
            , []
            )



-- === Card Insertion  ===


insert : String -> Int -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
insert pid pos initText ( model, prevCmd, prevMsgsToParent ) =
    let
        ( newIdString, newSeed ) =
            Random.step (stringGenerator 24) (GlobalData.seed model.globalData)

        newViewMode =
            case model.viewState.viewMode of
                FullscreenEditing _ ->
                    FullscreenEditing { cardId = newIdString, field = initText }

                _ ->
                    Editing { cardId = newIdString, field = initText }
    in
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Ins newIdString initText pid pos) model.workingTree
        , globalData = GlobalData.setSeed newSeed model.globalData
      }
    , prevCmd
    , prevMsgsToParent
        ++ [ LocalSave
                (CTIns newIdString
                    initText
                    (if pid == "0" then
                        Nothing

                     else
                        Just pid
                    )
                    pos
                )
           ]
    )
        |> andThen
            (changeMode
                { to = newViewMode
                , instant = False
                , save = False
                }
            )
        |> preventIfBlocked model


insertRelative : String -> Int -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
insertRelative id delta initText ( model, prevCmd, prevMsgsToParent ) =
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
            , prevMsgsToParent
            )
                |> insert pid (idx + delta) initText

        Nothing ->
            ( model
            , prevCmd
            , prevMsgsToParent
            )


insertAbove : String -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
insertAbove id initText tup =
    insertRelative id 0 initText tup


insertBelow : String -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
insertBelow id initText ( model, prevCmd, prevMsgsToParent ) =
    insertRelative id 1 initText ( model, prevCmd, prevMsgsToParent )


insertChild : String -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
insertChild id initText ( model, prevCmd, prevMsgsToParent ) =
    ( model
    , prevCmd
    , prevMsgsToParent
    )
        |> insert id 999999 initText


merge : Bool -> String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
merge isUp id ( model, prevCmd, prevMsgsToParent ) =
    let
        currentTree_ =
            getTree id model.workingTree.tree

        otherTree_ =
            if isUp then
                getPrevInColumn id model.workingTree.tree

            else
                getNextInColumn id model.workingTree.tree
    in
    case ( currentTree_, otherTree_ ) of
        ( Just currentTree, Just prevTree ) ->
            let
                mergedTree =
                    model.workingTree
                        |> TreeStructure.update (TreeStructure.Mrg currentTree prevTree isUp)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            , prevMsgsToParent
            )
                |> localSave (CTMrg currentTree.id prevTree.id isUp)
                |> addToHistory
                |> andThen (changeMode { to = Normal currentTree.id, instant = True, save = False })

        _ ->
            ( model, prevCmd, prevMsgsToParent )


mergeUp : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
mergeUp id ( model, prevCmd, prevMsgsToParent ) =
    merge True id ( model, prevCmd, prevMsgsToParent )


mergeDown : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
mergeDown id ( model, prevCmd, prevMsgsToParent ) =
    merge False id ( model, prevCmd, prevMsgsToParent )


setCursorPosition : Int -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
setCursorPosition pos ( model, prevCmd, prevMsgsToParent ) =
    ( model, Cmd.batch [ prevCmd, send (SetCursorPosition pos) ], prevMsgsToParent )



-- === Card Moving  ===


move : Tree -> String -> Int -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
move subtree pid pos ( model, prevCmd, prevMsgsToParent ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Mov subtree pid pos) model.workingTree
      }
    , prevCmd
    , prevMsgsToParent
    )
        |> activate subtree.id False
        |> localSave
            (CTMov subtree.id
                (if pid == "0" then
                    Nothing

                 else
                    Just pid
                )
                pos
            )
        |> addToHistory


moveWithin : String -> Int -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
moveWithin id delta ( model, prevCmd, prevMsgsToParent ) =
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
            , prevMsgsToParent
            )
                |> move tree pid (refIdx + delta |> Basics.max 0)

        _ ->
            ( model
            , prevCmd
            , prevMsgsToParent
            )


moveLeft : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
moveLeft id ( model, prevCmd, prevMsgsToParent ) =
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
            , prevMsgsToParent
            )
                |> move tree gpId (refIdx + 1)

        _ ->
            ( model
            , prevCmd
            , prevMsgsToParent
            )


moveRight : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
moveRight id ( model, prevCmd, prevMsgsToParent ) =
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
            , prevMsgsToParent
            )
                |> move tree prev 999999

        _ ->
            ( model
            , prevCmd
            , prevMsgsToParent
            )



-- === Card Cut/Copy/Paste ===


cut : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
cut id ( model, prevCmd, prevMsgsToParent ) =
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
        , prevCmd
        , ParentAddToast Temporary (Toast Warning "Cannot cut last card") :: prevMsgsToParent
        )

    else
        ( model, prevCmd, prevMsgsToParent )
            |> copy id
            |> deleteCard id


copy : String -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
copy id ( model, prevCmd, prevMsgsToParent ) =
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
    , prevMsgsToParent
    )


paste : Tree -> String -> Int -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
paste subtree pid pos ( model, prevCmd, prevMsgsToParent ) =
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Paste subtree pid pos) model.workingTree
      }
    , prevCmd
    , prevMsgsToParent
    )
        |> andThen (changeMode { to = Normal subtree.id, instant = True, save = False })
        |> localSave
            (if pid == "0" then
                CTBlk subtree Nothing pos

             else
                CTBlk subtree (Just pid) pos
            )
        |> addToHistory


pasteBelow : String -> Tree -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
pasteBelow id copiedTree ( model, prevCmd, prevMsgsToParent ) =
    let
        ( newId, newSeed ) =
            Random.step (stringGenerator 24) (GlobalData.seed model.globalData)

        treeToPaste =
            TreeStructure.renameNodes newId copiedTree

        pid =
            (getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0"

        pos =
            (getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1
    in
    ( { model | globalData = GlobalData.setSeed newSeed model.globalData }
    , prevCmd
    , prevMsgsToParent
    )
        |> paste treeToPaste pid pos


pasteInto : String -> Tree -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
pasteInto id copiedTree ( model, prevCmd, prevMsgsToParent ) =
    let
        ( newId, newSeed ) =
            Random.step (stringGenerator 24) (GlobalData.seed model.globalData)

        treeToPaste =
            TreeStructure.renameNodes newId copiedTree
    in
    ( { model | globalData = GlobalData.setSeed newSeed model.globalData }
    , prevCmd
    , prevMsgsToParent
    )
        |> paste treeToPaste id 999999



-- VIEW


type alias AppMsgs msg =
    { docMsg : Msg -> msg
    , keyboard : String -> msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }


view : AppMsgs msg -> Maybe Time.Posix -> Maybe Time.Posix -> Model -> List (Html msg)
view appMsg lastLocalSave lastRemoteSave (Model model) =
    if model.loading then
        UI.viewDocumentLoadingSpinner

    else
        case model.viewState.viewMode of
            FullscreenEditing { cardId, field } ->
                [ Fullscreen.view
                    { language = GlobalData.language model.globalData
                    , isMac = GlobalData.isMac model.globalData
                    , dirty = model.dirty
                    , lastLocalSave = lastLocalSave
                    , lastRemoteSave = lastRemoteSave
                    , currentTime = GlobalData.currentTime model.globalData
                    , model = model.workingTree
                    , activeId = cardId
                    , msgs =
                        { saveChanges = SaveFromFullscreen
                        , exitFullscreenRequested = ExitFullscreenRequested
                        , saveAndExitFullscreen = SaveAndExitFullscreen
                        }
                    }
                    |> Html.map appMsg.docMsg
                ]

            _ ->
                viewLoaded appMsg model


viewLoaded : AppMsgs msg -> ModelData -> List (Html msg)
viewLoaded ({ docMsg } as appMsg) model =
    let
        activeId =
            getActiveId (Model model)

        activeTree_ =
            getTree activeId model.workingTree.tree

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
                (case model.viewState.viewMode of
                    Normal _ ->
                        False

                    _ ->
                        True
                )
           , Keyed.node "div" [ style "display" "contents" ] [ ( "randomstringforloadingoverlay", div [ id "loading-overlay" ] [] ) ]
           , div [ id "preloader" ] []
           ]


treeView : Language -> Bool -> ViewState -> TreeStructure.Model -> Html Msg
treeView lang isMac vstate model =
    let
        activeId =
            getActiveIdFromViewState vstate

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
                        Normal _ ->
                            VisibleNormal

                        Editing _ ->
                            if c |> List.concat |> List.map .id |> List.member activeId then
                                VisibleEditing

                            else
                                VisibleNormal

                        FullscreenEditing _ ->
                            -- TODO : Impossible state
                            VisibleFullscreenEditing
            in
            VisibleViewState
                activeId
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
                        VisibleEditing ->
                            t.id == vstate.active

                        VisibleNormal ->
                            False

                        VisibleFullscreenEditing ->
                            -- TODO : Impossible state
                            False

                isLast =
                    t.id == lastChild

                collabsOnCard =
                    vstate.collaborators
                        |> List.filter (\c -> c.mode == CollabActive t.id || c.mode == CollabEditing t.id)
            in
            if isActive && not isEditing then
                ( t.id
                , lazy7 viewCardActive
                    vstate.language
                    t.id
                    t.content
                    (hasChildren t)
                    isLast
                    collabsOnCard
                    vstate.dragModel
                )

            else if isEditing then
                ( t.id
                , lazy5 viewCardEditing
                    vstate.language
                    t.id
                    t.content
                    (hasChildren t)
                    vstate.isMac
                )

            else
                ( t.id
                , lazy8 viewCardOther
                    t.id
                    t.content
                    collabsOnCard
                    isEditing
                    (hasChildren t)
                    isAncestor
                    isLast
                    vstate.dragModel
                )
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


viewCardOther : String -> String -> List Collaborator -> Bool -> Bool -> Bool -> Bool -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardOther cardId content collabsOnCard isEditing isParent isAncestor isLast dragModels =
    let
        collabsEditingCard =
            collabsOnCard |> List.filter (\c -> c.mode == CollabEditing cardId)
    in
    div
        [ id ("card-" ++ cardId)
        , dir "auto"
        , classList
            [ ( "card", True )
            , ( "ancestor", isAncestor )
            , ( "has-children", isParent )
            , ( "collab-active", not (List.isEmpty collabsOnCard) )
            , ( "collab-editing", not (List.isEmpty collabsEditingCard) )
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
               , UI.Collaborators.viewOnCard collabsOnCard
               ]
        )


viewCardActive : Language -> String -> String -> Bool -> Bool -> List Collaborator -> ( DragDrop.Model String DropId, DragExternalModel ) -> Html Msg
viewCardActive lang cardId content isParent isLast collabsOnCard dragModels =
    let
        collabsEditingCard =
            collabsOnCard |> List.filter (\c -> c.mode == CollabEditing cardId)

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
               , UI.Collaborators.viewOnCard collabsOnCard
               ]
        )


viewCardEditing : Language -> String -> String -> Bool -> Bool -> Html Msg
viewCardEditing lang cardId content isParent _ =
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
        [ node "gw-textarea"
            [ attribute "card-id" cardId
            , dir "auto"
            , classList
                [ ( "edit", True )
                , ( "mousetrap", True )
                ]
            , attribute "data-private" "lipsum"
            , attribute "data-gramm" "false"
            , attribute "start-value" content
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


preventIfBlocked : ModelData -> ( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )
preventIfBlocked originalModel ( newModel, cmd, parentMsgs ) =
    case originalModel.block of
        Nothing ->
            ( newModel, cmd, parentMsgs )

        Just blockReason ->
            ( originalModel, send <| Alert blockReason, [] )


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    Sub.batch
        [ if model.dirty then
            Time.every (241 * 1000) (always AutoSave)

          else
            Sub.none
        ]


setTree : Tree -> Model -> ( Model, Cmd Msg, List MsgToParent )
setTree tree (Model model) =
    { model
        | workingTree = TreeStructure.setTree tree model.workingTree
    }
        |> (\m -> ( m, Cmd.none, [] ))
        |> activate (getActiveId (Model model)) False
        |> (\( m, c, msgs ) -> ( Model m, c, msgs ))


setWorkingTree : TreeStructure.Model -> Model -> Model
setWorkingTree workingTree (Model model) =
    Model
        { model | workingTree = workingTree }



-- Temporary getters & setters until I move these fields to parent


isDirty : Model -> Bool
isDirty (Model model) =
    model
        |> .dirty


isFullscreen : Model -> Bool
isFullscreen (Model model) =
    model
        |> .viewState
        |> .viewMode
        |> (\vm ->
                case vm of
                    FullscreenEditing _ ->
                        True

                    _ ->
                        False
           )


isNormalMode : Model -> Bool
isNormalMode (Model model) =
    model
        |> .viewState
        |> .viewMode
        |> (\vm ->
                case vm of
                    Normal _ ->
                        True

                    _ ->
                        False
           )


getViewMode : Model -> ViewMode
getViewMode (Model model) =
    model
        |> .viewState
        |> .viewMode


getActiveId : Model -> String
getActiveId (Model model) =
    case model.viewState.viewMode of
        FullscreenEditing { cardId } ->
            cardId

        Editing { cardId } ->
            cardId

        Normal id ->
            id


getActiveIdFromViewMode : ViewMode -> String
getActiveIdFromViewMode viewMode =
    case viewMode of
        FullscreenEditing { cardId } ->
            cardId

        Editing { cardId } ->
            cardId

        Normal id ->
            id


getActiveIdFromViewState : ViewState -> String
getActiveIdFromViewState viewState =
    case viewState.viewMode of
        FullscreenEditing { cardId } ->
            cardId

        Editing { cardId } ->
            cardId

        Normal id ->
            id


lastActives : Result Json.Error (List String) -> Model -> ( Model, Cmd Msg )
lastActives activesResult (Model prevModel) =
    let
        vs =
            prevModel.viewState

        ( newViewState, maybeScroll ) =
            case activesResult of
                Ok (lastActive :: activePast) ->
                    let
                        newViewMode =
                            case vs.viewMode of
                                FullscreenEditing { field } ->
                                    FullscreenEditing { cardId = lastActive, field = field }

                                Editing { field } ->
                                    Editing { cardId = lastActive, field = field }

                                Normal _ ->
                                    Normal lastActive
                    in
                    ( { vs | viewMode = newViewMode, activePast = activePast }
                    , activate lastActive True
                    )

                Ok _ ->
                    ( vs, activate "1" True )

                Err _ ->
                    ( vs, identity )
    in
    ( { prevModel | viewState = newViewState }, Cmd.none, [] )
        |> maybeScroll
        |> (\( m, c, _ ) -> ( Model m, c ))


getTextCursorInfo : Model -> TextCursorInfo
getTextCursorInfo (Model model) =
    model
        |> .textCursorInfo


setDirty : Bool -> Model -> Model
setDirty dirty (Model model) =
    Model { model | dirty = dirty }


getGlobalData : Model -> GlobalData
getGlobalData (Model model) =
    model
        |> .globalData


setGlobalData : GlobalData -> Model -> Model
setGlobalData globalData (Model model) =
    Model
        { model
            | globalData = globalData
        }


setBlock : Maybe String -> Model -> Model
setBlock block_ (Model model) =
    Model { model | block = block_ }


maybeActivate : Model -> ( Model, Cmd Msg )
maybeActivate (Model model) =
    let
        activeId =
            getActiveId (Model model)
    in
    changeMode { to = Normal activeId, instant = False, save = False } model
        |> (\( m, c, _ ) -> ( Model m, c ))


setLoading : Bool -> Model -> Model
setLoading loading (Model model) =
    Model
        { model
            | loading = loading
        }


getWorkingTree : Model -> TreeStructure.Model
getWorkingTree (Model model) =
    model
        |> .workingTree


getCollaborators : Model -> List Collaborator
getCollaborators (Model model) =
    model
        |> .viewState
        |> .collaborators


getActiveTree : Model -> Maybe Tree
getActiveTree (Model model) =
    getTree (getActiveId (Model model)) model.workingTree.tree



-- HELPERS


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus ("card-edit-" ++ id))


normalMode : ModelData -> (( ModelData, Cmd Msg, List MsgToParent ) -> ( ModelData, Cmd Msg, List MsgToParent )) -> ( ModelData, Cmd Msg, List MsgToParent )
normalMode model operation =
    ( model
    , Cmd.none
    , []
    )
        |> (case model.viewState.viewMode of
                Normal _ ->
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
