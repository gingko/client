port module Main exposing (InitModel, Model)

import Browser
import Browser.Dom
import Coders exposing (..)
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, toDebouncer)
import Dict
import Fonts
import Fullscreen
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy, lazy3)
import Html5.DragDrop as DragDrop
import Json.Decode as Json
import List.Extra as ListExtra exposing (getAt)
import Objects
import Ports exposing (..)
import Random
import Regex
import Task
import Time
import Translation exposing (langFromString, tr)
import TreeUtils exposing (..)
import Trees exposing (..)
import Types exposing (..)
import UI exposing (countWords, viewFooter, viewSaveIndicator, viewSearchField, viewVideo)


main : Program ( Json.Value, InitModel ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{-
   # MODEL

   The Model contains the entire state of the document.

   The most important fields are `workingTree`, `viewState` and `objects`.

   `workingTree` contains the current state of the *document*, as it stands at
   any given moment. It does not include information about "transient" state
   (such as which card is focused, or which is being edited). It's defined in
   Trees.elm.

   `viewState` is where that "transient" information (focused card, edit state)
   is stored. It's defined in Types.elm.

   `objects` contains the current state of the version history data. It's what
   gets saved to the database. It's defined in Objects.elm.

-}


type alias Model =
    -- Document state
    { workingTree : Trees.Model
    , undoHistory : { before : List ( Trees.Model, String ), after : List ( Trees.Model, String ) }
    , docState : DocState
    , dirty : Bool

    -- Transient state
    , viewState : ViewState
    , field : String
    , textCursorInfo : TextCursorInfo
    , debouncerStateCommit : Debouncer () ()
    , debouncerEditing : Debouncer () ()
    , shortcutTrayOpen : Bool
    , wordcountTrayOpen : Bool
    , videoModalOpen : Bool
    , fontSelectorOpen : Bool
    , historyState : HistoryState
    , online : Bool

    -- Settings
    , uid : String
    , language : Translation.Language
    , isMac : Bool
    , fonts : Fonts.Model
    , startingWordcount : Int
    , currentTime : Time.Posix
    , seed : Random.Seed
    }



{-
   InitModel is a reduced form of the model that contains all the user settings
   that are loaded outside of Elm, and present at initialization.
-}


type alias InitModel =
    { filePath : Maybe String
    , backupPath : String
    , lastSaved : Maybe Float
    , language : String
    , isMac : Bool
    , shortcutTrayOpen : Bool
    , videoModalOpen : Bool
    , currentTime : Int
    , lastActive : String
    , fonts : Maybe ( String, String, String )
    }


defaultModel : Model
defaultModel =
    { workingTree = Trees.defaultModel
    , undoHistory = { before = [], after = [] }
    , docState = FileDoc (NewDoc "")
    , dirty = False
    , field = ""
    , debouncerStateCommit =
        Debouncer.throttle (fromSeconds 3)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 3)
            |> toDebouncer
    , debouncerEditing =
        Debouncer.debounce (fromSeconds 3)
            |> toDebouncer
    , uid = "0"
    , viewState =
        { active = "1"
        , viewMode = Normal
        , activePast = []
        , descendants = []
        , ancestors = [ "0" ]
        , searchField = Nothing
        , dragModel = DragDrop.init
        , draggedTree = Nothing
        , copiedTree = Nothing
        , collaborators = []
        }
    , textCursorInfo = { selected = False, position = End, text = ( "", "" ) }
    , isMac = False
    , language = Translation.En
    , shortcutTrayOpen = True
    , wordcountTrayOpen = False
    , videoModalOpen = False
    , fontSelectorOpen = False
    , fonts = Fonts.default
    , startingWordcount = 0
    , historyState = Closed
    , online = False
    , currentTime = Time.millisToPosix 0
    , seed = Random.initialSeed 12345
    }



{-
   init is where we load the model data upon initialization.
   If there is no such data, then we're starting a new document, and
   defaultModel is used instead.

   The dataIn is always JSON, but it can either be a JSON representation of the
   tree (from a .json file import), OR a full database load from a file
   containing the full commit history (a .gko file).
-}


init : ( Json.Value, InitModel ) -> ( Model, Cmd Msg )
init ( dataIn, modelIn ) =
    let
        ( newTree, docState ) =
            case Json.decodeValue treeDecoder dataIn of
                Ok newTreeDecoded ->
                    let
                        docStateIn =
                            case ( modelIn.filePath, modelIn.lastSaved ) of
                                ( Just filePath, Just lastSaved ) ->
                                    FileDoc
                                        (SavedDoc
                                            { filePath = filePath
                                            , lastSaved = (Time.millisToPosix << round) lastSaved
                                            }
                                        )

                                _ ->
                                    FileDoc (NewDoc modelIn.backupPath)
                    in
                    ( newTreeDecoded, docStateIn )

                Err err ->
                    ( Trees.defaultTree, FileDoc (NewDoc modelIn.backupPath) )

        newWorkingTree =
            Trees.setTree newTree defaultModel.workingTree

        startingWordcount =
            countWords (treeToMarkdownString False newTree)

        columnNumber =
            newWorkingTree.columns |> List.length |> (\l -> l - 1)
    in
    ( { defaultModel
        | workingTree = newWorkingTree
        , docState = docState
        , language = langFromString modelIn.language
        , isMac = modelIn.isMac
        , shortcutTrayOpen = modelIn.shortcutTrayOpen
        , videoModalOpen = modelIn.videoModalOpen
        , startingWordcount = startingWordcount
        , currentTime = Time.millisToPosix modelIn.currentTime
        , seed = Random.initialSeed modelIn.currentTime
        , fonts = Fonts.init modelIn.fonts
      }
    , Cmd.batch [ focus modelIn.lastActive, sendOut <| ColumnNumberChange columnNumber ]
    )
        |> activate modelIn.lastActive



{-
   # UPDATE

   Update is where we react to message events (Msg), and modify the model
   depending on what Msg was received.

   Most messages arise from within Elm itself, but some come into Elm from JS
   via ports.

   Each branch of the case statement returns the updated model, AND a piece of
   data called a command (Cmd) that describes an action for the Elm runtime to
   take. By far the most common such action is to sendOut an OutgoingMsg to JS.

   Most branches here call a function defined below, instead of updating the
   model in the update function itself.

   Msg, IncomingMsg, and OutgoingMsg can all be found in Types.elm.
-}


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ workingTree } as model) =
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
                |> activate id

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
                            activate id

                        ( Nothing, _ ) ->
                            activate vs.active

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

        OpenCardFullscreen id str ->
            ( model
            , Cmd.none
            )
                |> saveCardIfEditing
                |> openCardFullscreen id str

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
                    DragDrop.update dragDropMsg vs.dragModel

                modelDragUpdated =
                    { model
                        | viewState =
                            { vs
                                | dragModel = newDragModel
                            }
                    }
            in
            case ( DragDrop.getDragId newDragModel, dragResult_ ) of
                ( Just dragId, Nothing ) ->
                    -- Dragging
                    ( modelDragUpdated
                    , DragDrop.getDragstartEvent dragDropMsg
                        |> Maybe.map (.event >> dragstart)
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
                            ( { modelDragUpdated | viewState = { vs | draggedTree = Nothing }, dirty = True }, sendOut <| SetChanged True )
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

                ( Just dragId, Just _ ) ->
                    -- Should be Impossible: both Dragging and Dropped
                    ( modelDragUpdated, Cmd.none )

        -- === File System ===
        ThrottledSave subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerStateCommit

                mappedCmd =
                    Cmd.map ThrottledSave subCmd

                updatedModel =
                    { model | debouncerStateCommit = subModel }
            in
            case emitted_ of
                Just () ->
                    ( updatedModel
                    , mappedCmd
                    )
                        |> addToHistoryInstant model

                Nothing ->
                    ( updatedModel, mappedCmd )

        ThrottledSaveWIP subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerEditing

                mappedCmd =
                    Cmd.map ThrottledSaveWIP subCmd

                updatedModel =
                    { model | debouncerEditing = subModel }

                tempSavedTree =
                    saveCardIfEditing ( model, Cmd.none )
                        |> Tuple.first
                        |> .workingTree
                        |> .tree
            in
            case emitted_ of
                Just () ->
                    case updatedModel.docState of
                        FileDoc (SavedDoc { filePath }) ->
                            ( updatedModel
                            , Cmd.batch [ sendOut <| SaveSwap tempSavedTree filePath, mappedCmd ]
                            )

                        FileDoc (NewDoc backupPath) ->
                            ( updatedModel
                            , Cmd.batch [ sendOut <| SaveBackup tempSavedTree backupPath, mappedCmd ]
                            )

                        CloudDoc _ ->
                            ( updatedModel, mappedCmd )

                Nothing ->
                    ( updatedModel, mappedCmd )

        -- === UI ===
        TimeUpdate time ->
            ( { model | currentTime = time }
            , Cmd.none
            )

        VideoModal shouldOpen ->
            ( model
            , Cmd.none
            )
                |> toggleVideoModal shouldOpen

        FontsMsg fontsMsg ->
            let
                ( newModel, selectorOpen, newFontsTriple_ ) =
                    Fonts.update fontsMsg model.fonts

                cmd =
                    case newFontsTriple_ of
                        Just newFontsTriple ->
                            sendOut (SetFonts newFontsTriple)

                        Nothing ->
                            Cmd.none
            in
            ( { model | fonts = newModel, fontSelectorOpen = selectorOpen }
            , cmd
            )

        ShortcutTrayToggle ->
            let
                newIsOpen =
                    not model.shortcutTrayOpen
            in
            ( { model
                | shortcutTrayOpen = newIsOpen
              }
            , sendOut (SetShortcutTray newIsOpen)
            )

        WordcountTrayToggle ->
            ( { model | wordcountTrayOpen = not model.wordcountTrayOpen }
            , Cmd.none
            )

        -- === Ports ===
        Port incomingMsg ->
            case incomingMsg of
                -- === File States ===
                FileSave newFilePath_ ->
                    let
                        tempSavedTree =
                            saveCardIfEditing ( model, Cmd.none )
                                |> Tuple.first
                                |> .workingTree
                                |> .tree
                    in
                    case ( model.docState, newFilePath_ ) of
                        ( FileDoc (SavedDoc { filePath }), Nothing ) ->
                            ( model, sendOut <| SaveFile tempSavedTree filePath )

                        ( FileDoc (SavedDoc _), Just newFilePath ) ->
                            ( model, sendOut <| SaveFile tempSavedTree newFilePath )

                        ( FileDoc (NewDoc _), Just newFilePath ) ->
                            ( model, sendOut <| SaveFile tempSavedTree newFilePath )

                        ( FileDoc (NewDoc _), Nothing ) ->
                            ( model, Cmd.none )

                        ( CloudDoc _, _ ) ->
                            ( model, Cmd.none )

                SetLastSaved filePath mtime ->
                    case model.docState of
                        FileDoc _ ->
                            ( { model
                                | docState = FileDoc <| SavedDoc { filePath = filePath, lastSaved = mtime }
                                , dirty = False
                              }
                            , Cmd.none
                            )

                        CloudDoc _ ->
                            ( model, Cmd.none )

                -- === Dialogs, Menus, Window State ===
                IntentExport exportSettings ->
                    case exportSettings.format of
                        DOCX ->
                            let
                                markdownString m =
                                    case exportSettings.selection of
                                        All ->
                                            m.workingTree.tree
                                                |> treeToMarkdownString False

                                        CurrentSubtree ->
                                            getTree vs.active m.workingTree.tree
                                                |> Maybe.withDefault m.workingTree.tree
                                                |> treeToMarkdownString True

                                        ColumnNumber col ->
                                            getColumn col m.workingTree.tree
                                                |> Maybe.withDefault [ [] ]
                                                |> List.concat
                                                |> List.map .content
                                                |> String.join "\n\n"
                            in
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> (\( m, c ) ->
                                        ( m
                                        , Cmd.batch [ c, sendOut (ExportDOCX (markdownString m) exportSettings.filepath) ]
                                        )
                                   )

                        JSON ->
                            case exportSettings.selection of
                                All ->
                                    ( model
                                    , Cmd.none
                                    )
                                        |> saveCardIfEditing
                                        |> (\( m, c ) ->
                                                ( m
                                                , Cmd.batch [ c, sendOut (ExportJSON m.workingTree.tree exportSettings.filepath) ]
                                                )
                                           )

                                _ ->
                                    ( model
                                    , Cmd.none
                                    )

                        TXT ->
                            case exportSettings.selection of
                                All ->
                                    ( model
                                    , Cmd.none
                                    )
                                        |> saveCardIfEditing
                                        |> (\( m, c ) ->
                                                ( m
                                                , Cmd.batch [ c, sendOut (ExportTXT False m.workingTree.tree exportSettings.filepath) ]
                                                )
                                           )

                                CurrentSubtree ->
                                    let
                                        getCurrentSubtree m =
                                            getTree vs.active m.workingTree.tree
                                                |> Maybe.withDefault m.workingTree.tree
                                    in
                                    ( model
                                    , Cmd.none
                                    )
                                        |> saveCardIfEditing
                                        |> (\( m, c ) ->
                                                ( m
                                                , Cmd.batch [ c, sendOut (ExportTXT True (getCurrentSubtree m) exportSettings.filepath) ]
                                                )
                                           )

                                ColumnNumber col ->
                                    ( model
                                    , Cmd.none
                                    )
                                        |> saveCardIfEditing
                                        |> (\( m, c ) ->
                                                ( m
                                                , Cmd.batch [ c, sendOut (ExportTXTColumn col m.workingTree.tree exportSettings.filepath) ]
                                                )
                                           )

                CancelCardConfirmed ->
                    ( { model | dirty = False }
                    , Cmd.none
                    )
                        |> cancelCard

                -- === DOM ===
                DragStarted dragId ->
                    let
                        newTree =
                            Trees.update (Trees.Rmv dragId) model.workingTree

                        draggedTree =
                            getTreeWithPosition dragId model.workingTree.tree
                    in
                    if List.isEmpty <| getChildren newTree.tree then
                        ( model, Cmd.none )

                    else
                        ( { model | workingTree = newTree, viewState = { vs | draggedTree = draggedTree } }, Cmd.none )

                FieldChanged str ->
                    ( { model
                        | field = str
                        , dirty = True
                      }
                    , Cmd.none
                    )
                        |> saveWIPThrottled

                TextCursor textCursorInfo ->
                    if model.textCursorInfo /= textCursorInfo then
                        ( { model | textCursorInfo = textCursorInfo }
                        , Cmd.none
                        )

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
                                    Trees.update (Trees.Upd cardId newContent) model.workingTree
                            in
                            ( { model | workingTree = newTree, dirty = True }, Cmd.none )
                                |> addToHistoryInstant model

                -- === UI ===
                SetLanguage lang ->
                    ( { model | language = lang }
                    , Cmd.none
                    )

                ViewVideos ->
                    ( model
                    , Cmd.none
                    )
                        |> toggleVideoModal True

                FontSelectorOpen fonts ->
                    ( { model | fonts = Fonts.setSystem fonts model.fonts, fontSelectorOpen = True }
                    , Cmd.none
                    )

                Keyboard shortcut ->
                    case shortcut of
                        "shift+enter" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> (\( m, c ) ->
                                        case vs.viewMode of
                                            Normal ->
                                                openCardFullscreen vs.active (getContent vs.active m.workingTree.tree) ( m, c )

                                            _ ->
                                                closeCard ( m, c )
                                   )
                                |> activate vs.active

                        "mod+enter" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> (\( m, c ) ->
                                        case vs.viewMode of
                                            Normal ->
                                                openCard vs.active (getContent vs.active m.workingTree.tree) ( m, c )

                                            _ ->
                                                closeCard ( m, c )
                                   )
                                |> activate vs.active

                        "enter" ->
                            normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

                        "mod+backspace" ->
                            normalMode model (deleteCard vs.active)

                        "esc" ->
                            model |> intentCancelCard

                        "mod+j" ->
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

                        "alt+home" ->
                            normalMode model (moveWithin vs.active -999999)

                        "alt+end" ->
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

                        "mod+v" ->
                            normalMode model (pasteBelow vs.active)

                        "mod+shift+v" ->
                            normalMode model (pasteInto vs.active)

                        "mod+z" ->
                            normalMode model (historyStep Backward)

                        "mod+shift+z" ->
                            normalMode model (historyStep Forward)

                        "mod+s" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing

                        "mod+b" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , sendOut (TextSurround vs.active "**")
                                    )

                        "mod+i" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , sendOut (TextSurround vs.active "*")
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
                            case vs.viewMode of
                                Normal ->
                                    ( { model | wordcountTrayOpen = not model.wordcountTrayOpen }
                                    , Cmd.none
                                    )

                                _ ->
                                    ( model
                                    , Cmd.none
                                    )

                        _ ->
                            let
                                _ =
                                    Debug.log "unhandled shortcut" shortcut
                            in
                            ( model
                            , Cmd.none
                            )

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
                                    Trees.update (Trees.Upd editId collabState.field) model.workingTree

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

        LogErr err ->
            ( model
            , sendOut (ConsoleLogRequested err)
            )

        NoOp ->
            ( model
            , Cmd.none
            )



-- === Card Activation ===


activate : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
activate id ( model, prevCmd ) =
    let
        vs =
            model.viewState
    in
    if id == "0" then
        ( model
        , prevCmd
        )

    else
        let
            activeTree_ =
                getTree id model.workingTree.tree

            newPast =
                if id == vs.active then
                    vs.activePast

                else
                    vs.active :: vs.activePast |> List.take 40
        in
        case activeTree_ of
            Just activeTree ->
                let
                    desc =
                        activeTree
                            |> getDescendants
                            |> List.map .id

                    anc =
                        getAncestors model.workingTree.tree activeTree []
                            |> List.map .id

                    flatCols =
                        model.workingTree.columns
                            |> List.map (\c -> List.map (\g -> List.map .id g) c)
                            |> List.map List.concat

                    newField =
                        activeTree.content

                    allIds =
                        anc
                            ++ [ id ]
                            ++ desc
                in
                ( { model
                    | viewState =
                        { vs
                            | active = id
                            , activePast = newPast
                            , descendants = desc
                            , ancestors = anc
                        }
                    , field = newField
                  }
                , Cmd.batch
                    [ prevCmd
                    , sendOut
                        (ActivateCards
                            ( id
                            , getDepth 0 model.workingTree.tree id
                            , centerlineIds flatCols allIds newPast
                            )
                        )
                    ]
                )

            Nothing ->
                ( model
                , prevCmd
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
        |> activate targetId


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
        |> activate targetId


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
        |> activate targetId


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

        Just tree ->
            if List.length childrenIds == 0 then
                ( model
                , prevCmd
                )

            else
                ( model
                , prevCmd
                )
                    |> activate prevActiveOfChildren



-- === Card Editing  ===


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
                    Trees.update (Trees.Upd vs.active model.field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                  }
                , prevCmd
                )
                    |> addToHistoryInstant model

            else
                ( { model | dirty = False }
                , Cmd.batch [ prevCmd, sendOut <| SetChanged False ]
                )


openCard : String -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
openCard id str ( model, prevCmd ) =
    let
        vs =
            model.viewState

        isLocked =
            vs.collaborators
                |> List.filter (\c -> c.mode == CollabEditing id)
                |> (not << List.isEmpty)

        isHistoryView =
            model.historyState /= Closed
    in
    if isHistoryView then
        ( model
        , Cmd.batch [ prevCmd, sendOut (Alert "Cannot edit while browsing version history.") ]
        )

    else if isLocked then
        ( model
        , Cmd.batch [ prevCmd, sendOut (Alert "Card is being edited by someone else.") ]
        )

    else
        ( { model
            | viewState = { vs | active = id, viewMode = Editing }
            , field = str
          }
        , Cmd.batch [ prevCmd, focus id ]
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
                ( { m | viewState = { vs | active = id, viewMode = FullscreenEditing }, field = str }, c )
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

        filteredActive =
            vs.activePast
                |> List.filter (\a -> a /= id)

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
        , sendOut (Alert "Card is being edited by someone else.")
        )

    else if isLastChild then
        ( model
        , sendOut (Alert "Cannot delete last card.")
        )

    else
        ( { model
            | workingTree = Trees.update (Trees.Rmv id) model.workingTree
            , dirty = True
          }
        , Cmd.batch [ prevCmd, sendOut <| SetChanged True ]
        )
            |> maybeColumnsChanged model.workingTree.columns
            |> activate nextToActivate
            |> addToHistoryInstant model


goToTopOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToTopOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getFirstInColumn id model.workingTree.tree)


goToBottomOfColumn : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
goToBottomOfColumn id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> activate (getLastInColumn id model.workingTree.tree)


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
        |> activate targetId


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
        |> activate targetId


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
            , sendOut (ConfirmCancelCard vs.active originalContent)
            )



-- === Card Insertion  ===


insert : String -> Int -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insert pid pos initText ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomId model.seed

        newIdString =
            "node-" ++ (newId |> Debug.toString)
    in
    ( { model
        | workingTree = Trees.update (Trees.Ins newIdString initText pid pos) model.workingTree
        , seed = newSeed
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> openCard newIdString initText
        |> activate newIdString


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
insertBelow id initText (( model, cmd ) as tup) =
    insertRelative id 1 initText tup


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
                        |> Trees.update (Trees.Mrg currentTree prevTree True)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate prevTree.id
                |> addToHistoryInstant model

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
                        |> Trees.update (Trees.Mrg currentTree nextTree False)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate nextTree.id
                |> addToHistoryInstant model

        _ ->
            ( model, prevCmd )


maybeColumnsChanged : List Column -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
maybeColumnsChanged oldColumns ( { workingTree } as model, prevCmd ) =
    let
        oldColNumber =
            oldColumns |> List.length

        newColNumber =
            workingTree.columns |> List.length

        colsChangedCmd =
            if newColNumber /= oldColNumber then
                sendOut (ColumnNumberChange (newColNumber - 1))

            else
                Cmd.none
    in
    ( model
    , Cmd.batch [ prevCmd, colsChangedCmd ]
    )


setCursorPosition : Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
setCursorPosition pos ( model, prevCmd ) =
    ( model, Cmd.batch [ prevCmd, sendOut (SetCursorPosition pos) ] )



-- === Card Moving  ===


move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = Trees.update (Trees.Mov subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id
        |> addToHistoryThrottled


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
        , sendOut (Alert "Cannot cut last card")
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
    in
    ( { model
        | viewState = { vs | copiedTree = getTree id model.workingTree.tree }
      }
    , Cmd.batch
        [ prevCmd
        , sendOut FlashCurrentSubtree
        ]
    )


paste : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
paste subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = Trees.update (Trees.Paste subtree pid pos) model.workingTree
        , dirty = True
      }
    , Cmd.batch [ sendOut <| SetChanged True, prevCmd ]
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id
        |> addToHistoryInstant model


pasteBelow : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteBelow id ( model, prevCmd ) =
    case model.viewState.copiedTree of
        Just copiedTree ->
            let
                vs =
                    model.viewState

                ( newId, newSeed ) =
                    Random.step randomId model.seed

                treeToPaste =
                    Trees.renameNodes (newId |> String.fromInt) copiedTree

                pid =
                    (getParent id model.workingTree.tree |> Maybe.map .id) |> Maybe.withDefault "0"

                pos =
                    (getIndex id model.workingTree.tree |> Maybe.withDefault 0) + 1
            in
            ( { model | seed = newSeed }
            , prevCmd
            )
                |> paste treeToPaste pid pos

        Nothing ->
            ( model
            , prevCmd
            )


pasteInto : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteInto id ( model, prevCmd ) =
    case model.viewState.copiedTree of
        Just copiedTree ->
            let
                vs =
                    model.viewState

                ( newId, newSeed ) =
                    Random.step randomId model.seed

                treeToPaste =
                    Trees.renameNodes (newId |> String.fromInt) copiedTree
            in
            ( { model | seed = newSeed }
            , prevCmd
            )
                |> paste treeToPaste id 999999

        Nothing ->
            ( model
            , prevCmd
            )



-- === History ===


addToHistoryInstant : Model -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryInstant oldModel ( { workingTree, currentTime } as model, prevCmd ) =
    let
        newBefore =
            ( oldModel.workingTree, model.viewState.active )
                :: model.undoHistory.before

        newCmds =
            case model.docState of
                FileDoc (SavedDoc { filePath }) ->
                    Cmd.batch [ sendOut (SaveSwap model.workingTree.tree filePath), prevCmd ]

                FileDoc (NewDoc backupPath) ->
                    Cmd.batch [ sendOut (SaveBackup model.workingTree.tree backupPath), prevCmd ]

                CloudDoc _ ->
                    prevCmd
    in
    ( { model
        | undoHistory = { before = newBefore, after = [] }
      }
    , newCmds
    )


addToHistoryThrottled : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryThrottled ( model, prevCmd ) =
    update (ThrottledSave (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])


saveWIPThrottled : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
saveWIPThrottled ( model, prevCmd ) =
    update (ThrottledSaveWIP (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])


historyStep : Direction -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
historyStep dir ( model, prevCmd ) =
    let
        newCmds =
            case model.docState of
                FileDoc (SavedDoc { filePath }) ->
                    Cmd.batch [ sendOut (SaveSwap model.workingTree.tree filePath), prevCmd ]

                _ ->
                    prevCmd
    in
    case dir of
        Backward ->
            case ListExtra.uncons model.undoHistory.before of
                Just ( bef, newBefore ) ->
                    ( { model
                        | workingTree = Tuple.first bef
                        , undoHistory =
                            { before = newBefore
                            , after =
                                ( model.workingTree, model.viewState.active ) :: model.undoHistory.after
                            }
                      }
                    , newCmds
                    )
                        |> activate (Tuple.second bef)

                Nothing ->
                    ( model, prevCmd )

        Forward ->
            case ListExtra.uncons model.undoHistory.after of
                Just ( aft, newAfter ) ->
                    ( { model
                        | workingTree = Tuple.first aft
                        , undoHistory =
                            { before =
                                ( model.workingTree, model.viewState.active ) :: model.undoHistory.before
                            , after =
                                newAfter
                            }
                      }
                    , newCmds
                    )
                        |> activate (Tuple.second aft)

                Nothing ->
                    ( model, prevCmd )



-- === Files ===


toggleVideoModal : Bool -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
toggleVideoModal shouldOpen ( model, prevCmd ) =
    ( { model
        | videoModalOpen = shouldOpen
      }
    , Cmd.batch [ prevCmd, sendOut (SetVideoModal shouldOpen) ]
    )



-- VIEW


view : Model -> Html Msg
view model =
    let
        replace orig new =
            Regex.replace (Regex.fromString orig |> Maybe.withDefault Regex.never) (\_ -> new)

        styleNode =
            node "style"
                []
                [ text
                    ("""
h1, h2, h3, h4, h5, h6 {
  font-family: '@HEADING', serif;
}
.card .view {
  font-family: '@CONTENT', sans-serif;
}
pre, code, .group.has-active .card textarea {
  font-family: '@MONOSPACE', monospace;
}
"""
                        |> replace "@HEADING" (Fonts.getHeading model.fonts)
                        |> replace "@CONTENT" (Fonts.getContent model.fonts)
                        |> replace "@MONOSPACE" (Fonts.getMonospace model.fonts)
                    )
                ]
    in
    if model.viewState.viewMode == FullscreenEditing then
        div
            [ id "app-root" ]
            [ if model.fontSelectorOpen then
                Fonts.viewSelector model.language model.fonts |> Html.map FontsMsg

              else
                text ""
            , lazy3 Fullscreen.view model.language model.viewState model.workingTree
            ]

    else
        div
            [ id "app-root" ]
            [ if model.fontSelectorOpen then
                Fonts.viewSelector model.language model.fonts |> Html.map FontsMsg

              else
                text ""
            , lazy3 Trees.view model.language model.viewState model.workingTree
            , viewSaveIndicator model
            , viewSearchField model
            , viewFooter model
            , viewVideo model
            , styleNode
            ]



-- SUBSCRIPTIONS


port dragstart : Json.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveMsg Port LogErr
        , Time.every (15 * 1000) TimeUpdate
        ]



-- HELPERS


randomId : Random.Generator Int
randomId =
    Random.int 0 Random.maxInt


getHead : Status -> Maybe String
getHead status =
    case status of
        Clean head ->
            Just head

        MergeConflict _ head _ [] ->
            Just head

        _ ->
            Nothing


focus : String -> Cmd Msg
focus id =
    Task.attempt (\_ -> NoOp) (Browser.Dom.focus ("card-edit-" ++ id))


run : Msg -> Cmd Msg
run msg =
    Task.attempt (\_ -> msg) (Task.succeed msg)


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
