port module Main exposing (InitModel, Model)

import Browser
import Browser.Dom
import Coders exposing (..)
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, toDebouncer)
import Dict
import Fonts
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
import UI exposing (countWords, viewConflict, viewFooter, viewHistory, viewSaveIndicator, viewSearchField, viewVideo)


main : Program ( Json.Value, InitModel, Bool ) Model Msg
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

   Complete list of fields:

   workingTree : Current state of the document

   objects : The history data

   status : Status of the history = Bare | Clean ... | MergeConflict ...

   debouncerStateCommit : Debouncer is used to prevent trying to commit too often

   debouncerStateBackup : Longer debouncer for less frequent actions (save backup)

   uid : Unique user/device id, for realtime collaboration.

   viewState : Stores which card is active, edit mode, which is being dragged, etc.

   field : Edited card's field value. Updated by onChange handler in the textarea.

   isMac : Set at init, allows us to show mac-specific content (e.g. ⌘ in shortcuts)

   isTextSelected : Set by JS, allows us to show text-selection-only shortcuts (e.g. Bold).

   shortcutTrayOpen : Is the small shortcut reference tray open or closed?

   videoModalOpen : Is the video tutorial modal open or closed? (Unused for now).

   startingWordcount : Word count on open. To see how many words were written in this session.

   historyState : State for the display of version history = Closed | From startingSha

   online : Are we online or not, in order to attempt to sync. (Unused for now).

   changed : Has the document been changed since load?

   currentTime : Updated every 15 seconds. For use in , e.g. "Last saved X minutes ago".
-}


type alias Model =
    { workingTree : Trees.Model
    , objects : Objects.Model
    , status : Status
    , debouncerStateCommit : Debouncer () ()
    , debouncerStateBackup : Debouncer () ()
    , uid : String
    , viewState : ViewState
    , field : String
    , language : Translation.Language
    , isMac : Bool
    , isTextSelected : Bool
    , shortcutTrayOpen : Bool
    , wordcountTrayOpen : Bool
    , videoModalOpen : Bool
    , fontSelectorOpen : Bool
    , fonts : Fonts.Model
    , startingWordcount : Int
    , historyState : HistoryState
    , online : Bool
    , changed : Bool
    , currentTime : Time.Posix
    , seed : Random.Seed
    }



{-
   InitModel is a reduced form of the model that contains all the user settings
   that are loaded outside of Elm, and present at initialization.
-}


type alias InitModel =
    { language : String
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
    , objects = Objects.defaultModel
    , status = Bare
    , debouncerStateCommit =
        Debouncer.throttle (fromSeconds 3)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 3)
            |> toDebouncer
    , debouncerStateBackup =
        Debouncer.debounce (fromSeconds 40)
            |> Debouncer.settleWhenQuietFor (Just <| fromSeconds 40)
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
    , field = ""
    , isMac = False
    , language = Translation.En
    , isTextSelected = False
    , shortcutTrayOpen = True
    , wordcountTrayOpen = False
    , videoModalOpen = False
    , fontSelectorOpen = False
    , fonts = Fonts.default
    , startingWordcount = 0
    , historyState = Closed
    , online = False
    , changed = False
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


init : ( Json.Value, InitModel, Bool ) -> ( Model, Cmd Msg )
init ( dataIn, modelIn, isSaved ) =
    let
        ( newStatus, newTree_, newObjects ) =
            case Json.decodeValue treeDecoder dataIn of
                Ok newTreeDecoded ->
                    {- The JSON was successfully decoded by treeDecoder.
                       We need to create the first commit to the history.
                    -}
                    Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" modelIn.currentTime newTreeDecoded) defaultModel.objects
                        |> (\( s, _, o ) -> ( s, Just newTreeDecoded, o ))

                Err err ->
                    {- If treeDecoder fails, we assume that this was a
                       load from the database instead. See Objects.elm for
                       how the data is converted from JSON to type Objects.Model
                    -}
                    Objects.update (Objects.Init dataIn) defaultModel.objects

        newTree =
            Maybe.withDefault Trees.defaultTree newTree_

        newWorkingTree =
            Trees.setTree newTree defaultModel.workingTree

        startingWordcount =
            newTree_
                |> Maybe.map (\t -> countWords (treeToMarkdownString False t))
                |> Maybe.withDefault 0
    in
    ( { defaultModel
        | workingTree = newWorkingTree
        , objects = newObjects
        , status = newStatus
        , language = langFromString modelIn.language
        , isMac = modelIn.isMac
        , shortcutTrayOpen = modelIn.shortcutTrayOpen
        , videoModalOpen = modelIn.videoModalOpen
        , startingWordcount = startingWordcount
        , currentTime = Time.millisToPosix modelIn.currentTime
        , seed = Random.initialSeed modelIn.currentTime
        , fonts = Fonts.init modelIn.fonts
      }
    , Cmd.batch [ focus modelIn.lastActive, sendOut <| ColumnNumberChange <| List.length <| newWorkingTree.columns ]
    )
        |> activate modelIn.lastActive
        |> (\mc ->
                if not isSaved then
                    mc |> addToHistory

                else
                    mc
           )



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
update msg ({ objects, workingTree, status } as model) =
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
                |> insertAbove id

        InsertBelow id ->
            ( model
            , Cmd.none
            )
                |> insertBelow id

        InsertChild id ->
            ( model
            , Cmd.none
            )
                |> insertChild id

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
                            ( { modelDragUpdated | viewState = { vs | draggedTree = Nothing } }, Cmd.none )
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

        -- === History ===
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
                    , sendOut CommitWithTimestamp
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        ThrottledBackup subMsg ->
            let
                ( subModel, subCmd, emitted_ ) =
                    Debouncer.update subMsg model.debouncerStateBackup

                mappedCmd =
                    Cmd.map ThrottledBackup subCmd

                updatedModel =
                    { model | debouncerStateBackup = subModel }
            in
            case emitted_ of
                Just () ->
                    ( updatedModel
                    , sendOut SaveBackup
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        CheckoutCommit commitSha ->
            case status of
                MergeConflict _ _ _ _ ->
                    ( model
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )
                        |> checkoutCommit commitSha

        Restore ->
            ( { model | historyState = Closed }
            , Cmd.none
            )
                |> addToHistoryDo

        CancelHistory ->
            case model.historyState of
                From origSha ->
                    ( { model | historyState = Closed }
                    , Cmd.none
                    )
                        |> checkoutCommit origSha

                Closed ->
                    ( model
                    , Cmd.none
                    )

        Sync ->
            case ( model.status, model.online ) of
                ( Clean _, True ) ->
                    ( model
                    , sendOut Pull
                    )

                ( Bare, True ) ->
                    ( model
                    , sendOut Pull
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        SetSelection cid selection id ->
            let
                newStatus =
                    case status of
                        MergeConflict mTree oldHead newHead conflicts ->
                            conflicts
                                |> List.map
                                    (\c ->
                                        if c.id == cid then
                                            { c | selection = selection }

                                        else
                                            c
                                    )
                                |> MergeConflict mTree oldHead newHead

                        _ ->
                            status
            in
            case newStatus of
                MergeConflict mTree oldHead newHead conflicts ->
                    case selection of
                        Manual ->
                            ( { model
                                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
                                , status = newStatus
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( { model
                                | workingTree = Trees.setTreeWithConflicts conflicts mTree model.workingTree
                                , status = newStatus
                              }
                            , Cmd.none
                            )
                                |> cancelCard
                                |> activate id

                _ ->
                    ( model
                    , Cmd.none
                    )

        Resolve cid ->
            case status of
                MergeConflict mTree shaA shaB conflicts ->
                    ( { model
                        | status = MergeConflict mTree shaA shaB (conflicts |> List.filter (\c -> c.id /= cid))
                      }
                    , Cmd.none
                    )
                        |> addToHistory

                _ ->
                    ( model
                    , Cmd.none
                    )

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
                -- === Dialogs, Menus, Window State ===
                IntentSave ->
                    ( model
                    , Cmd.none
                    )
                        |> saveCardIfEditing

                IntentExit ->
                    case vs.viewMode of
                        Editing ->
                            let
                                modelCardSaved =
                                    ( model
                                    , Cmd.none
                                    )
                                        |> saveCardIfEditing
                                        |> Tuple.first

                                ( statusValue, objectsValue ) =
                                    ( statusToValue modelCardSaved.status, Objects.toValue modelCardSaved.objects )
                            in
                            ( model
                            , sendOut (SaveAndClose (Just ( statusValue, objectsValue )))
                            )

                        Normal ->
                            ( model
                            , sendOut (SaveAndClose Nothing)
                            )

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
                    ( model
                    , Cmd.none
                    )
                        |> cancelCard

                -- === Database ===
                Commit timeMillis ->
                    ( { model | currentTime = Time.millisToPosix timeMillis }, Cmd.none )
                        |> addToHistoryDo

                SetHeadRev rev ->
                    ( { model
                        | objects = Objects.setHeadRev rev model.objects
                        , changed = False
                      }
                    , Cmd.none
                    )
                        |> push

                Merge json ->
                    let
                        ( newStatus, newTree_, newObjects ) =
                            Objects.update (Objects.Merge json workingTree.tree) objects
                    in
                    case ( status, newStatus ) of
                        ( Bare, Clean sha ) ->
                            ( { model
                                | workingTree = Trees.setTree (newTree_ |> Maybe.withDefault workingTree.tree) workingTree
                                , objects = newObjects
                                , status = newStatus
                              }
                            , sendOut (UpdateCommits ( Objects.toValue newObjects, Just sha ))
                            )
                                |> activate vs.active

                        ( Clean oldHead, Clean newHead ) ->
                            if oldHead /= newHead then
                                ( { model
                                    | workingTree = Trees.setTree (newTree_ |> Maybe.withDefault workingTree.tree) workingTree
                                    , objects = newObjects
                                    , status = newStatus
                                  }
                                , sendOut (UpdateCommits ( Objects.toValue newObjects, Just newHead ))
                                )
                                    |> activate vs.active

                            else
                                ( model
                                , Cmd.none
                                )

                        ( Clean _, MergeConflict mTree oldHead newHead conflicts ) ->
                            ( { model
                                | workingTree =
                                    if List.isEmpty conflicts then
                                        Trees.setTree (newTree_ |> Maybe.withDefault workingTree.tree) workingTree

                                    else
                                        Trees.setTreeWithConflicts conflicts mTree model.workingTree
                                , objects = newObjects
                                , status = newStatus
                              }
                            , sendOut (UpdateCommits ( newObjects |> Objects.toValue, Just newHead ))
                            )
                                |> addToHistory
                                |> activate vs.active

                        _ ->
                            let
                                _ =
                                    Debug.log "failed to merge json" json
                            in
                            ( model
                            , Cmd.none
                            )

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
                        , changed = True
                      }
                    , Cmd.none
                    )

                TextSelected isSel ->
                    ( { model | isTextSelected = isSel }
                    , Cmd.none
                    )

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
                        "mod+enter" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> (\( m, c ) ->
                                        case vs.viewMode of
                                            Normal ->
                                                openCard vs.active (getContent vs.active m.workingTree.tree) ( m, c )

                                            Editing ->
                                                ( m, c )
                                   )
                                |> activate vs.active

                        "enter" ->
                            normalMode model (openCard vs.active (getContent vs.active model.workingTree.tree))

                        "mod+backspace" ->
                            normalMode model (deleteCard vs.active)

                        "esc" ->
                            model |> intentCancelCard

                        "mod+j" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> insertBelow vs.active

                        "mod+down" ->
                            normalMode model (insertBelow vs.active)

                        "mod+k" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> insertAbove vs.active

                        "mod+up" ->
                            normalMode model (insertAbove vs.active)

                        "mod+l" ->
                            ( model
                            , Cmd.none
                            )
                                |> saveCardIfEditing
                                |> insertChild vs.active

                        "mod+right" ->
                            normalMode model (insertChild vs.active)

                        "h" ->
                            normalMode model (goLeft vs.active)

                        "left" ->
                            normalMode model (goLeft vs.active)

                        "j" ->
                            normalMode model (goDown vs.active)

                        "down" ->
                            normalMode model (goDown vs.active)

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

                                Editing ->
                                    ( model
                                    , sendOut (TextSurround vs.active "**")
                                    )

                        "mod+i" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Cmd.none
                                    )

                                Editing ->
                                    ( model
                                    , sendOut (TextSurround vs.active "*")
                                    )

                        "/" ->
                            case vs.viewMode of
                                Normal ->
                                    ( model
                                    , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "search-input")
                                    )

                                Editing ->
                                    ( model
                                    , Cmd.none
                                    )

                        "w" ->
                            case vs.viewMode of
                                Normal ->
                                    ( { model | wordcountTrayOpen = not model.wordcountTrayOpen }
                                    , Cmd.none
                                    )

                                Editing ->
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
                    |> sendCollabState (CollabState model.uid (CollabActive id) "")

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
        Editing ->
            let
                newTree =
                    Trees.update (Trees.Upd vs.active model.field) model.workingTree
            in
            if newTree.tree /= model.workingTree.tree then
                ( { model
                    | workingTree = newTree
                    , viewState = { vs | viewMode = Normal }
                    , field = ""
                  }
                , prevCmd
                )
                    |> addToHistory

            else
                ( { model
                    | viewState = { vs | viewMode = Normal }
                    , field = ""
                    , changed = False
                  }
                , Cmd.batch [ prevCmd, sendOut (SetChanged False) ]
                )

        Normal ->
            ( model
            , prevCmd
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
            |> sendCollabState (CollabState model.uid (CollabEditing id) str)


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
          }
        , prevCmd
        )
            |> maybeColumnsChanged model.workingTree.columns
            |> activate nextToActivate
            |> addToHistory


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
        , changed = False
        , field = ""
      }
    , prevCmd
    )
        |> sendCollabState (CollabState model.uid (CollabActive vs.active) "")


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

        Editing ->
            ( model
            , sendOut (ConfirmCancelCard vs.active originalContent)
            )



-- === Card Insertion  ===


insert : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insert pid pos ( model, prevCmd ) =
    let
        ( newId, newSeed ) =
            Random.step randomId model.seed

        newIdString =
            "node-" ++ (newId |> Debug.toString)
    in
    ( { model
        | workingTree = Trees.update (Trees.Ins newIdString "" pid pos) model.workingTree
        , seed = newSeed
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> openCard newIdString ""
        |> activate newIdString


insertRelative : String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertRelative id delta ( model, prevCmd ) =
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
                |> insert pid (idx + delta)

        Nothing ->
            ( model
            , prevCmd
            )


insertAbove : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertAbove id tup =
    insertRelative id 0 tup


insertBelow : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertBelow id tup =
    insertRelative id 1 tup


insertChild : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
insertChild id ( model, prevCmd ) =
    ( model
    , prevCmd
    )
        |> insert id 999999


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



-- === Card Moving  ===


move : Tree -> String -> Int -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
move subtree pid pos ( model, prevCmd ) =
    ( { model
        | workingTree = Trees.update (Trees.Mov subtree pid pos) model.workingTree
        , changed = True
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id
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
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id
        |> addToHistory


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


checkoutCommit : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
checkoutCommit commitSha ( model, prevCmd ) =
    let
        ( newStatus, newTree_, newModel ) =
            Objects.update (Objects.Checkout commitSha) model.objects
    in
    case newTree_ of
        Just newTree ->
            ( { model
                | workingTree = Trees.setTree newTree model.workingTree
                , status = newStatus
              }
            , sendOut (UpdateCommits ( Objects.toValue model.objects, getHead newStatus ))
            )
                |> maybeColumnsChanged model.workingTree.columns

        Nothing ->
            ( model
            , Cmd.none
            )
                |> Debug.log "failed to load commit"


historyStep : Direction -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
historyStep dir ( model, prevCmd ) =
    case model.status of
        Clean currHead ->
            let
                master =
                    Dict.get "heads/master" model.objects.refs

                ( currCommit_, historyList ) =
                    case master of
                        Just refObj ->
                            ( Just refObj.value
                            , (refObj.value :: refObj.ancestors)
                                |> List.reverse
                            )

                        _ ->
                            ( Nothing, [] )

                newCommitIdx_ =
                    case dir of
                        Backward ->
                            historyList
                                |> ListExtra.elemIndex currHead
                                |> Maybe.map (\x -> Basics.max 0 (x - 1))
                                |> Maybe.withDefault -1

                        Forward ->
                            historyList
                                |> ListExtra.elemIndex currHead
                                |> Maybe.map (\x -> Basics.min (List.length historyList - 1) (x + 1))
                                |> Maybe.withDefault -1

                newCommit_ =
                    getAt newCommitIdx_ historyList
            in
            case ( model.historyState, currCommit_, newCommit_ ) of
                ( From startSha, _, Just newSha ) ->
                    ( model
                    , prevCmd
                    )
                        |> checkoutCommit newSha

                ( Closed, Just currCommit, Just newSha ) ->
                    ( { model | historyState = From currCommit }
                    , prevCmd
                    )
                        |> checkoutCommit newSha

                _ ->
                    ( model, prevCmd )

        _ ->
            ( model, prevCmd )


push : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
push ( model, prevCmd ) =
    if model.online then
        ( model
        , Cmd.batch [ prevCmd, sendOut Push ]
        )

    else
        ( model
        , prevCmd
        )


addToHistoryDo : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryDo ( { workingTree, currentTime } as model, prevCmd ) =
    case model.status of
        Bare ->
            let
                ( newStatus, _, newObjects ) =
                    Objects.update (Objects.Commit [] "Jane Doe <jane.doe@gmail.com>" (currentTime |> Time.posixToMillis) workingTree.tree) model.objects
            in
            ( { model
                | objects = newObjects
                , status = newStatus
                , changed = True
              }
            , Cmd.batch
                [ prevCmd
                , sendOut (SaveToDB ( statusToValue newStatus, Objects.toValue newObjects ))
                , sendOut (UpdateCommits ( Objects.toValue newObjects, getHead newStatus ))
                ]
            )

        Clean oldHead ->
            let
                ( newStatus, _, newObjects ) =
                    Objects.update (Objects.Commit [ oldHead ] "Jane Doe <jane.doe@gmail.com>" (currentTime |> Time.posixToMillis) workingTree.tree) model.objects
            in
            ( { model
                | objects = newObjects
                , status = newStatus
                , changed = True
              }
            , Cmd.batch
                [ prevCmd
                , sendOut (SaveToDB ( statusToValue newStatus, Objects.toValue newObjects ))
                , sendOut (UpdateCommits ( Objects.toValue newObjects, getHead newStatus ))
                ]
            )

        MergeConflict _ oldHead newHead conflicts ->
            if List.isEmpty conflicts || (conflicts |> List.filter (not << .resolved) |> List.isEmpty) then
                let
                    ( newStatus, _, newObjects ) =
                        Objects.update (Objects.Commit [ oldHead, newHead ] "Jane Doe <jane.doe@gmail.com>" (currentTime |> Time.posixToMillis) workingTree.tree) model.objects
                in
                ( { model
                    | objects = newObjects
                    , status = newStatus
                    , changed = True
                  }
                , Cmd.batch
                    [ prevCmd
                    , sendOut (SaveToDB ( statusToValue newStatus, Objects.toValue newObjects ))
                    , sendOut (UpdateCommits ( Objects.toValue newObjects, getHead newStatus ))
                    ]
                )

            else
                ( model
                , Cmd.batch [ prevCmd, sendOut (SaveLocal model.workingTree.tree) ]
                )


addToHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistory ( model, prevCmd ) =
    update (ThrottledCommit (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])
        |> (\( mdl, cmd ) -> update (ThrottledBackup (provideInput ())) mdl |> Tuple.mapSecond (\c -> Cmd.batch [ cmd, c ]))



-- === Files ===


sendCollabState : CollabState -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendCollabState collabState ( model, prevCmd ) =
    case model.status of
        MergeConflict _ _ _ _ ->
            ( model
            , prevCmd
            )

        _ ->
            ( model
            , Cmd.batch [ prevCmd, sendOut (SocketSend collabState) ]
            )


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
    case model.status of
        MergeConflict _ oldHead newHead conflicts ->
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
            div
                [ id "app-root"
                , style "background" bgString
                , style "position" "absolute"
                , style "width" "100%"
                , style "height" "100%"
                ]
                [ ul [ class "conflicts-list" ]
                    (List.map viewConflict conflicts)
                , lazy3 Trees.view model.language model.viewState model.workingTree
                , styleNode
                ]

        _ ->
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
                , case ( model.historyState, model.status ) of
                    ( From _, Clean currHead ) ->
                        viewHistory model.language currHead model.objects

                    _ ->
                        text ""
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

                Editing ->
                    identity
           )
