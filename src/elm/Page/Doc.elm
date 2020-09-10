port module Page.Doc exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Browser.Dom
import Bytes exposing (Bytes)
import Coders exposing (treeToMarkdownString)
import Debouncer.Basic as Debouncer exposing (Debouncer, fromSeconds, provideInput, toDebouncer)
import Doc.Data as Data
import Doc.Data.Conflict exposing (Selection)
import Doc.Fonts as Fonts
import Doc.Fullscreen as Fullscreen
import Doc.Metadata as Metadata exposing (Metadata)
import Doc.TreeStructure as TreeStructure exposing (defaultTree)
import Doc.TreeUtils exposing (..)
import Doc.UI as UI exposing (countWords, viewConflict, viewFooter, viewHistory, viewSearchField, viewVideo)
import File.Download as Download
import Html exposing (Html, a, button, div, h1, input, node, span, text, textarea, ul)
import Html.Attributes exposing (autofocus, class, classList, dir, href, id, style, title, value)
import Html.Events exposing (onClick, onDoubleClick, onInput)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy2, lazy3)
import Html5.DragDrop as DragDrop
import Http
import Json.Decode as Json
import List.Extra as ListExtra exposing (getAt)
import Markdown
import Ports exposing (ExportFormat(..), ExportSelection(..), IncomingMsg(..), OutgoingMsg(..), receiveMsg, sendOut)
import Random
import Regex
import Route
import Session exposing (Session)
import Task
import Time
import Translation exposing (..)
import Tuple exposing (first, second)
import Types exposing (Children(..), CollabState, Column, CursorPosition(..), DropId(..), Group, Mode(..), TextCursorInfo, Tree, ViewMode(..), ViewState, VisibleViewState)



-- MODEL


type alias Model =
    -- Document state
    { workingTree : TreeStructure.Model
    , data : Data.Model
    , metadata : Metadata

    -- SPA Page State
    , session : Session
    , loading : Bool

    -- Transient state
    , viewState : ViewState
    , dirty : Bool
    , lastLocalSave : Maybe Time.Posix
    , lastRemoteSave : Maybe Time.Posix
    , field : String
    , textCursorInfo : TextCursorInfo
    , debouncerStateCommit : Debouncer () ()
    , titleField : Maybe String
    , sidebarOpen : Bool
    , accountMenuOpen : Bool
    , shortcutTrayOpen : Bool
    , wordcountTrayOpen : Bool
    , videoModalOpen : Bool
    , fontSelectorOpen : Bool
    , historyState : HistoryState

    -- Settings
    , uid : String
    , language : Translation.Language
    , isMac : Bool
    , fonts : Fonts.Model
    , startingWordcount : Int
    , currentTime : Time.Posix
    , seed : Random.Seed
    }


defaultModel : Bool -> Session -> String -> Model
defaultModel isNew session docId =
    { workingTree = TreeStructure.defaultModel
    , data = Data.empty
    , metadata = Metadata.new docId
    , session = session
    , loading = not isNew
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
        , dragModel = DragDrop.init
        , draggedTree = Nothing
        , copiedTree = Nothing
        , collaborators = []
        }
    , dirty = False
    , lastLocalSave = Nothing
    , lastRemoteSave = Nothing
    , field = ""
    , textCursorInfo = { selected = False, position = End, text = ( "", "" ) }
    , isMac = False
    , language = Translation.En
    , titleField = Nothing
    , sidebarOpen = False
    , accountMenuOpen = False
    , shortcutTrayOpen = False -- TODO
    , wordcountTrayOpen = False
    , videoModalOpen = False
    , fontSelectorOpen = False
    , fonts = Fonts.default
    , startingWordcount = 0
    , historyState = Closed
    , currentTime = Time.millisToPosix 0
    , seed = Random.initialSeed (Session.seed session)
    }


init : Session -> String -> Bool -> ( Model, Cmd Msg )
init session dbName isNew =
    ( defaultModel isNew session dbName
    , if not isNew then
        sendOut <| LoadDocument dbName

      else
        sendOut <| InitDocument dbName
    )
        |> (if isNew then
                activate "1" True

            else
                identity
           )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = NoOp
      -- === Card Activation ===
    | Activate String
    | SearchFieldUpdated String
      -- === Card Editing  ===
    | OpenCard String String
    | OpenCardFullscreen String String
    | DeleteCard String
      -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
      -- === Card Moving  ===
    | DragDropMsg (DragDrop.Msg String DropId)
      -- === History ===
    | ThrottledCommit (Debouncer.Msg ())
    | Commit Time.Posix
    | CheckoutCommit String
    | Restore
    | CancelHistory
    | PullFromRemote
    | SetSelection String Selection String
    | Resolve String
      -- === UI ===
    | ToggledTitleEdit Bool
    | TitleFieldChanged String
    | TitleEdited
    | ToggledAccountMenu Bool
    | ToggledSidebar Bool
    | TimeUpdate Time.Posix
    | VideoModal Bool
    | FontsMsg Fonts.Msg
    | ShortcutTrayToggle
    | WordcountTrayToggle
      -- === Ports ===
    | ExportAll
    | Exported (Result Http.Error Bytes)
    | Port IncomingMsg
    | LogErr String


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
                    , Cmd.batch [ Task.perform Commit Time.now, mappedCmd ]
                    )

                Nothing ->
                    ( updatedModel, mappedCmd )

        Commit time ->
            ( { model | currentTime = time }, Cmd.none )
                |> addToHistoryDo

        CheckoutCommit commitSha ->
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

        PullFromRemote ->
            case ( vs.viewMode, Data.conflictList model.data ) of
                ( Normal, [] ) ->
                    ( model
                    , sendOut Pull
                    )

                _ ->
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
                |> addToHistory

        -- === UI ===
        ToggledTitleEdit isEditingTitle ->
            if isEditingTitle then
                ( { model
                    | titleField = Metadata.getDocName model.metadata |> Maybe.withDefault "" |> Just
                  }
                , Cmd.none
                )

            else
                ( { model | titleField = Nothing }
                , Cmd.none
                )

        TitleFieldChanged newTitle ->
            ( { model | titleField = Just newTitle }, Cmd.none )

        TitleEdited ->
            case model.titleField of
                Just editedTitle ->
                    if Just editedTitle /= Metadata.getDocName model.metadata then
                        ( model, sendOut <| SaveMetadata <| Metadata.rename editedTitle model.metadata )

                    else
                        ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        ToggledAccountMenu isOpen ->
            ( { model | accountMenuOpen = isOpen }, Cmd.none )

        ToggledSidebar sidebarOpen ->
            ( { model | sidebarOpen = sidebarOpen }, Cmd.none )

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
        ExportAll ->
            let
                markdownString =
                    treeToMarkdownString False model.workingTree.tree
            in
            ( model, Api.exportDocx Exported { docId = Metadata.getDocId model.metadata, markdown = markdownString } )

        Exported (Ok bytes) ->
            let
                mime =
                    "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            in
            ( model, Download.bytes "test.docx" mime bytes )

        Exported (Err _) ->
            ( model, Cmd.none )

        Port incomingMsg ->
            case incomingMsg of
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
                    , sendOut <| Pull
                    )

                DataReceived dataIn ->
                    dataReceived dataIn model

                UserStoreLoaded dataIn ->
                    let
                        userSettingsDecoder =
                            Json.map2 (\st l -> { shortcutTrayOpen = st, language = l |> langFromString })
                                (Json.field "shortcut-tray-is-open" Json.bool)
                                (Json.field "language" Json.string)
                    in
                    case Json.decodeValue userSettingsDecoder dataIn of
                        Ok settings ->
                            ( { model
                                | shortcutTrayOpen = settings.shortcutTrayOpen
                                , language = settings.language
                              }
                            , Cmd.none
                            )

                        Err _ ->
                            ( model, Cmd.none )

                LocalStoreLoaded dataIn ->
                    case Json.decodeValue (Json.field "last-active" Json.string) dataIn of
                        Ok lastActive ->
                            ( { model | viewState = { vs | active = lastActive } }, Cmd.none )

                        Err _ ->
                            ( model, Cmd.none )

                MetadataSynced json ->
                    case Json.decodeValue Metadata.decoder json of
                        Ok metadata ->
                            ( { model | metadata = metadata }, Cmd.none )

                        Err err ->
                            ( model, Cmd.none )

                MetadataSaved json ->
                    case Json.decodeValue Metadata.decoder json of
                        Ok metadata ->
                            ( { model | titleField = Nothing, metadata = metadata }, Cmd.none )

                        Err err ->
                            ( model, Cmd.none )

                MetadataSaveError ->
                    ( { model | titleField = Nothing }, Cmd.none )

                GetDataToSave ->
                    case vs.viewMode of
                        Normal ->
                            ( model
                            , Cmd.none
                              --, sendOut (SaveToDB ( Metadata.encode model.metadata, statusToValue model.status, Data.encode model.objects ))
                            )

                        _ ->
                            let
                                newTree =
                                    TreeStructure.update (TreeStructure.Upd vs.active model.field) model.workingTree
                            in
                            if newTree.tree /= model.workingTree.tree then
                                ( { model | workingTree = newTree }
                                , Cmd.none
                                )
                                    |> addToHistoryDo

                            else
                                ( model
                                , Cmd.none
                                  --, sendOut (SaveToDB ( Metadata.encode model.metadata, statusToValue model.status, Data.encode model.objects ))
                                )

                SavedLocally time_ ->
                    ( { model
                        | lastLocalSave = time_
                      }
                    , Cmd.none
                    )

                SavedRemotely time_ ->
                    ( { model
                        | lastRemoteSave = time_
                      }
                    , Cmd.none
                    )

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

                FieldChanged str ->
                    ( { model
                        | field = str
                        , dirty = True
                      }
                    , Cmd.none
                    )

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
                                    TreeStructure.update (TreeStructure.Upd cardId newContent) model.workingTree
                            in
                            ( { model | workingTree = newTree, dirty = True }, Cmd.none )
                                |> addToHistory

                -- === UI ===
                LanguageChanged lang ->
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
                                |> activate vs.active False

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
                                |> activate vs.active False

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
                            case model.historyState of
                                From currHead ->
                                    normalMode model (historyStep Backward currHead)

                                Closed ->
                                    ( model, Cmd.none )

                        "mod+shift+z" ->
                            case model.historyState of
                                From currHead ->
                                    normalMode model (historyStep Forward currHead)

                                Closed ->
                                    ( model, Cmd.none )

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

                -- === UNUSED ===
                ImportComplete ->
                    ( model, Cmd.none )

                DocListChanged ->
                    ( model, Cmd.none )

        LogErr err ->
            ( model
            , sendOut (ConsoleLogRequested err)
            )

        NoOp ->
            ( model
            , Cmd.none
            )



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

            newPast =
                if tryId == vs.active then
                    vs.activePast

                else
                    vs.active :: vs.activePast |> List.take 40
        in
        case activeTree_ of
            Just activeTree ->
                let
                    id =
                        activeTree.id

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
                            instant
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

        Just tree ->
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
                    |> addToHistory

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
            |> sendCollabState (CollabState model.uid (CollabEditing id) str)


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
            | workingTree = TreeStructure.update (TreeStructure.Rmv id) model.workingTree
            , dirty = True
          }
        , Cmd.batch [ prevCmd, sendOut <| SetChanged True ]
        )
            |> maybeColumnsChanged model.workingTree.columns
            |> activate nextToActivate False
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
            "node-" ++ (newId |> String.fromInt)
    in
    ( { model
        | workingTree = TreeStructure.update (TreeStructure.Ins newIdString initText pid pos) model.workingTree
        , seed = newSeed
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> openCard newIdString initText
        |> activate newIdString True


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
                        |> TreeStructure.update (TreeStructure.Mrg currentTree prevTree True)
            in
            ( { model
                | workingTree = mergedTree
              }
            , prevCmd
            )
                |> activate prevTree.id False
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
                |> addToHistory

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
        | workingTree = TreeStructure.update (TreeStructure.Mov subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id False
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
        | workingTree = TreeStructure.update (TreeStructure.Paste subtree pid pos) model.workingTree
      }
    , prevCmd
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> activate subtree.id False
        |> addToHistory


pasteBelow : String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
pasteBelow id ( model, prevCmd ) =
    case model.viewState.copiedTree of
        Just copiedTree ->
            let
                ( newId, newSeed ) =
                    Random.step randomId model.seed

                treeToPaste =
                    TreeStructure.renameNodes (newId |> String.fromInt) copiedTree

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
                ( newId, newSeed ) =
                    Random.step randomId model.seed

                treeToPaste =
                    TreeStructure.renameNodes (newId |> String.fromInt) copiedTree
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
        newTree_ =
            Data.checkout commitSha model.data
    in
    case newTree_ of
        Just newTree ->
            ( { model
                | workingTree = TreeStructure.setTree newTree model.workingTree
              }
            , Cmd.none
              --, sendOut (UpdateCommits ( Data.encode model.objects, getHead newStatus ))
            )
                |> maybeColumnsChanged model.workingTree.columns

        Nothing ->
            ( model
            , prevCmd
            )



-- History


type Direction
    = Forward
    | Backward


type HistoryState
    = Closed
    | From String


historyStep : Direction -> String -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
historyStep dir currHead ( model, prevCmd ) =
    let
        master =
            Data.head "heads/master" model.data

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
        ( From _, _, Just newSha ) ->
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


addToHistoryDo : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistoryDo ( { workingTree, currentTime } as model, prevCmd ) =
    let
        newData =
            Data.commit "Jane Doe <jane.doe@gmail.com" (currentTime |> Time.posixToMillis) workingTree.tree model.data
    in
    if newData /= model.data then
        ( { model | data = newData }, sendOut <| SaveData (Data.encode newData) (Metadata.encode model.metadata) )

    else
        ( model, Cmd.none )


addToHistory : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
addToHistory ( model, prevCmd ) =
    update (ThrottledCommit (provideInput ())) model
        |> Tuple.mapSecond (\cmd -> Cmd.batch [ prevCmd, cmd ])



-- === Files ===


dataReceived : Json.Value -> Model -> ( Model, Cmd Msg )
dataReceived dataIn model =
    let
        { newModel, newTree, shouldPush } =
            Data.received dataIn ( model.data, model.workingTree.tree )

        newWorkingTree =
            TreeStructure.setTreeWithConflicts (Data.conflictList newModel) newTree model.workingTree

        startingWordcount =
            countWords (treeToMarkdownString False newTree)
    in
    ( { model
        | data = newModel
        , loading = False
        , workingTree = newWorkingTree
        , lastLocalSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
        , lastRemoteSave = Data.lastCommitTime newModel |> Maybe.map Time.millisToPosix
        , startingWordcount = startingWordcount
      }
    , Cmd.batch
        [ if shouldPush then
            sendOut <| Push

          else
            Cmd.none
        ]
    )
        |> maybeColumnsChanged model.workingTree.columns
        |> (if model.loading then
                activate model.viewState.active True

            else
                identity
           )


sendCollabState : CollabState -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
sendCollabState collabState ( model, prevCmd ) =
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
    if model.loading then
        div [ id "app-root", class "loading" ]
            ([ UI.viewHomeLink False
             , div [ id "document-header" ] []
             , div [ id "loading-overlay" ] []
             ]
                ++ UI.viewSidebar { exportAll = ExportAll, toggledSidebar = ToggledSidebar } model.sidebarOpen
            )

    else
        viewLoaded model


viewLoaded : Model -> Html Msg
viewLoaded model =
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
    case Data.conflictList model.data of
        [] ->
            if model.viewState.viewMode == FullscreenEditing then
                div
                    [ id "app-root" ]
                    [ if model.fontSelectorOpen then
                        Fonts.viewSelector model.language model.fonts |> Html.map FontsMsg

                      else
                        text ""
                    , lazy3 (Fullscreen.view OpenCardFullscreen) model.language model.viewState model.workingTree
                    ]

            else
                div
                    [ id "app-root" ]
                    ([ UI.viewHomeLink model.sidebarOpen
                     , lazy3 treeView model.language model.viewState model.workingTree
                     , UI.viewHeader { toggledTitleEdit = ToggledTitleEdit, titleFieldChanged = TitleFieldChanged, titleEdited = TitleEdited, toggledAccountMenu = ToggledAccountMenu }
                        (Metadata.getDocName model.metadata)
                        model
                     ]
                        ++ UI.viewSidebar { exportAll = ExportAll, toggledSidebar = ToggledSidebar } model.sidebarOpen
                        ++ [ viewSearchField SearchFieldUpdated model
                           , viewFooter WordcountTrayToggle ShortcutTrayToggle model
                           , case model.historyState of
                                From currHead ->
                                    viewHistory NoOp CheckoutCommit Restore CancelHistory model.language currHead model.data

                                _ ->
                                    text ""
                           , viewVideo VideoModal model
                           , styleNode
                           , div [ id "loading-overlay" ] []
                           ]
                    )

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
            div
                [ id "app-root"
                , style "background" bgString
                , style "position" "absolute"
                , style "width" "100%"
                , style "height" "100%"
                ]
                [ ul [ class "conflicts-list" ]
                    (List.map (viewConflict SetSelection Resolve) conflicts)
                , lazy3 treeView model.language model.viewState model.workingTree
                , styleNode
                ]


treeView : Language -> ViewState -> TreeStructure.Model -> Html Msg
treeView lang vstate model =
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
                    case vstate.viewMode of
                        Normal ->
                            Normal

                        Editing ->
                            if first cwd |> List.concat |> List.map .id |> List.member vstate.active then
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

        columns =
            [ ( [ [] ], -1 ) ]
                ++ columnsWithDepth
                ++ [ ( [ [] ], List.length columnsWithDepth ) ]
                |> List.map (\t -> lazy3 viewColumn (getViewArgs t) (second t) (first t))
    in
    div
        [ id "document"
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
            if t.id == vstate.active && not isEditing then
                ( t.id, viewCardActive vstate.language t.id t.content (hasChildren t) isLast collabsOnCard collabsEditingCard vstate.dragModel )

            else if isEditing then
                ( t.id, viewCardEditing vstate.language t.id t.content (hasChildren t) )

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
        (dropRegions cardId isEditing isLast dragModel
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardActive : Language -> String -> String -> Bool -> Bool -> List String -> List String -> DragDrop.Model String DropId -> Html Msg
viewCardActive lang cardId content isParent isLast collabsOnCard collabsEditingCard dragModel =
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
            ++ dropRegions cardId False isLast dragModel
            ++ [ div
                    [ class "view"
                    , onClick (Activate cardId)
                    , onDoubleClick (OpenCard cardId content)
                    ]
                    [ lazy2 viewContent cardId content ]
               , collabsSpan collabsOnCard collabsEditingCard
               ]
        )


viewCardEditing : Language -> String -> String -> Bool -> Html Msg
viewCardEditing lang cardId content isParent =
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
            , autofocus True
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
                , title <| tr lang SaveChangesTitle
                , onClick (Port (Keyboard "mod+enter"))
                ]
                []
            ]
        ]


viewHeader : Bool -> String -> Maybe String -> Html Msg
viewHeader isEditing headingField docName_ =
    if isEditing then
        div [ style "position" "fixed", style "right" "100px" ]
            [ input [ onInput TitleFieldChanged, value headingField ] []
            , button [ onClick TitleEdited ] [ text "Rename" ]
            ]

    else
        case docName_ of
            Just docName ->
                h1 [ onClick (ToggledTitleEdit True), style "position" "fixed", style "right" "100px" ] [ text docName ]

            Nothing ->
                h1 [ onClick (ToggledTitleEdit True), style "position" "fixed", style "right" "100px" ] [ text "Untitled" ]



-- HELPERS


hasChildren : Tree -> Bool
hasChildren { children } =
    case children of
        Children c ->
            (c
                |> List.length
            )
                /= 0


dropRegions : String -> Bool -> Bool -> DragDrop.Model String DropId -> List (Html Msg)
dropRegions cardId isEditing isLast dragModel =
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
    in
    case ( dragId_, isEditing ) of
        ( Just dragId, False ) ->
            [ dropDiv "above" (Above cardId)
            , dropDiv "into" (Into cardId)
            ]
                ++ (if isLast then
                        [ dropDiv "below" (Below cardId) ]

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



-- SUBSCRIPTIONS


port dragstart : Json.Value -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMsg Port LogErr
        , Time.every (9 * 1000) TimeUpdate
        , Time.every (10 * 1000) (\_ -> PullFromRemote)
        ]



-- HELPERS


randomId : Random.Generator Int
randomId =
    Random.int 0 Random.maxInt


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
