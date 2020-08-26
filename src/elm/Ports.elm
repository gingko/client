port module Ports exposing (ExportFormat(..), ExportSelection(..), ExportSettings, IncomingMsg(..), OutgoingMsg(..), encodeAndSend, infoForElm, infoForOutside, receiveMsg, sendOut, unionTypeToString)

import Coders exposing (..)
import Doc.Fonts as Fonts
import Doc.TreeUtils exposing (getColumn)
import Json.Decode as Dec exposing (Decoder, decodeValue, errorToString, field, oneOf)
import Json.Encode as Enc exposing (..)
import Json.Encode.Extra exposing (maybe)
import Time
import Translation exposing (languageDecoder)
import Types exposing (CollabState, CursorPosition(..), TextCursorInfo, Tree)



-- TYPES


type
    OutgoingMsg
    -- === Dialogs, Menus, Window State ===
    = Alert String
    | SetChanged Bool
    | ConfirmCancelCard String String
    | ColumnNumberChange Int
      -- === Database ===
    | LoadDocument String
    | CommitWithTimestamp
    | NoDataToSave
    | SaveData Enc.Value
    | Push
    | Pull
      -- === File System ===
    | ExportDOCX String (Maybe String)
    | ExportJSON Tree (Maybe String)
    | ExportTXT Bool Tree (Maybe String)
    | ExportTXTColumn Int Tree (Maybe String)
      -- === DOM ===
    | ActivateCards ( String, Int, List (List String) )
    | FlashCurrentSubtree
    | TextSurround String String
    | SetCursorPosition Int
      -- === UI ===
    | SaveMetadata Enc.Value
    | UpdateCommits ( Enc.Value, Maybe String )
    | SetVideoModal Bool
    | SetFonts Fonts.Settings
    | SetShortcutTray Bool
      -- === Misc ===
    | SocketSend CollabState
    | ConsoleLogRequested String


type
    IncomingMsg
    -- === Dialogs, Menus, Window State ===
    = IntentExport ExportSettings
    | CancelCardConfirmed
      -- === Database ===
    | DataSaved Dec.Value
    | DataReceived Dec.Value
    | UserStoreLoaded Dec.Value
    | LocalStoreLoaded Dec.Value
    | Commit Int
    | GetDataToSave
    | SavedLocally (Maybe Time.Posix)
    | SavedRemotely (Maybe Time.Posix)
      -- === Metadata ===
    | MetadataSynced Dec.Value
    | MetadataSaved Dec.Value
    | MetadataSaveError
      -- === DOM ===
    | DragStarted String
    | FieldChanged String
    | TextCursor TextCursorInfo
    | CheckboxClicked String Int
      -- === UI ===
    | LanguageChanged Translation.Language
    | ViewVideos
    | FontSelectorOpen (List String)
    | Keyboard String
      -- === Misc ===
    | RecvCollabState CollabState
    | CollaboratorDisconnected String


type alias OutsideData =
    { tag : String, data : Enc.Value }


type alias ExportSettings =
    { format : ExportFormat
    , selection : ExportSelection
    , filepath : Maybe String
    }


type ExportFormat
    = DOCX
    | JSON
    | TXT


type ExportSelection
    = All
    | CurrentSubtree
    | ColumnNumber Int



-- HELPERS


sendOut : OutgoingMsg -> Cmd msg
sendOut info =
    let
        dataToSend =
            encodeAndSend info
    in
    case info of
        -- === Dialogs, Menus, Window State ===
        Alert str ->
            dataToSend (string str)

        SetChanged changed ->
            dataToSend (bool changed)

        ConfirmCancelCard id origContent ->
            dataToSend (list string [ id, origContent ])

        ColumnNumberChange cols ->
            dataToSend (int cols)

        -- === Database ===
        SaveData data ->
            dataToSend data

        LoadDocument dbName ->
            dataToSend (string dbName)

        CommitWithTimestamp ->
            dataToSend null

        NoDataToSave ->
            dataToSend null

        Push ->
            dataToSend null

        Pull ->
            dataToSend null

        -- === File System ===
        ExportDOCX str path_ ->
            dataToSend
                (object
                    [ ( "data", string str )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        ExportJSON tree path_ ->
            dataToSend
                (object
                    [ ( "data", treeToJSON tree )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        ExportTXT withRoot tree path_ ->
            dataToSend
                (object
                    [ ( "data", treeToMarkdown withRoot tree )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        -- ExportTXTColumn is handled by 'ExportTXT' in JS
        -- So we use the "ExportTXT" tag here, instead of `dataToSend`
        ExportTXTColumn col tree path_ ->
            infoForOutside
                { tag = "ExportTXT"
                , data =
                    object
                        [ ( "data"
                          , tree
                                |> getColumn col
                                |> Maybe.withDefault [ [] ]
                                |> List.concat
                                |> List.map .content
                                |> String.join "\n\n"
                                |> string
                          )
                        , ( "filepath", maybe string path_ )
                        ]
                }

        -- === DOM ===
        ActivateCards ( cardId, col, lastActives ) ->
            let
                listListStringToValue lls =
                    list (list string) lls
            in
            dataToSend
                (object
                    [ ( "cardId", string cardId )
                    , ( "column", int col )
                    , ( "lastActives", listListStringToValue lastActives )
                    ]
                )

        FlashCurrentSubtree ->
            dataToSend null

        TextSurround id str ->
            dataToSend (list string [ id, str ])

        SetCursorPosition pos ->
            dataToSend (int pos)

        -- === UI ===
        SaveMetadata metadata ->
            dataToSend metadata

        UpdateCommits ( objectsValue, head_ ) ->
            let
                headToValue mbs =
                    case mbs of
                        Just str ->
                            string str

                        Nothing ->
                            null
            in
            dataToSend (tupleToValue identity ( objectsValue, headToValue head_ ))

        SetVideoModal isOpen ->
            dataToSend (bool isOpen)

        SetFonts fontSettings ->
            dataToSend (fontSettingsEncoder fontSettings)

        SetShortcutTray isOpen ->
            dataToSend (bool isOpen)

        -- === Misc ===
        SocketSend collabState ->
            dataToSend (collabStateToValue collabState)

        ConsoleLogRequested err ->
            dataToSend (string err)


receiveMsg : (IncomingMsg -> msg) -> (String -> msg) -> Sub msg
receiveMsg tagger onError =
    infoForElm
        (\outsideInfo ->
            case outsideInfo.tag of
                -- === Dialogs, Menus, Window State ===
                "IntentExport" ->
                    case decodeValue exportSettingsDecoder outsideInfo.data of
                        Ok exportSettings ->
                            tagger <| IntentExport exportSettings

                        Err e ->
                            onError (errorToString e)

                "CancelCardConfirmed" ->
                    tagger <| CancelCardConfirmed

                -- === Database ===
                "DataSaved" ->
                    tagger <| DataSaved outsideInfo.data

                "DataReceived" ->
                    tagger <| DataReceived outsideInfo.data

                "UserStoreLoaded" ->
                    tagger <| UserStoreLoaded outsideInfo.data

                "LocalStoreLoaded" ->
                    tagger <| LocalStoreLoaded outsideInfo.data

                "MetadataSynced" ->
                    tagger <| MetadataSynced outsideInfo.data

                "MetadataSaved" ->
                    tagger <| MetadataSaved outsideInfo.data

                "Commit" ->
                    case decodeValue Dec.int outsideInfo.data of
                        Ok time ->
                            tagger <| Commit time

                        Err e ->
                            onError (errorToString e)

                "GetDataToSave" ->
                    tagger <| GetDataToSave

                "SavedLocally" ->
                    case decodeValue (Dec.maybe Dec.int) outsideInfo.data of
                        Ok time_ ->
                            tagger <| SavedLocally (Maybe.map Time.millisToPosix time_)

                        Err e ->
                            onError (errorToString e)

                "SavedRemotely" ->
                    case decodeValue (Dec.maybe Dec.float) outsideInfo.data of
                        Ok time_ ->
                            tagger <| SavedRemotely (Maybe.map (Time.millisToPosix << round) time_)

                        Err e ->
                            onError (errorToString e)

                -- === DOM ===
                "DragStarted" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok dragId ->
                            tagger <| DragStarted dragId

                        Err e ->
                            onError (errorToString e)

                "FieldChanged" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok newField ->
                            tagger <| FieldChanged newField

                        Err e ->
                            onError (errorToString e)

                "TextCursor" ->
                    case decodeValue textCursorInfoDecoder outsideInfo.data of
                        Ok textCursorInfo ->
                            tagger <| TextCursor textCursorInfo

                        Err e ->
                            onError (errorToString e)

                "CheckboxClicked" ->
                    case decodeValue (tupleDecoder Dec.string Dec.int) outsideInfo.data of
                        Ok ( cardId, checkboxNumber ) ->
                            tagger <| CheckboxClicked cardId checkboxNumber

                        Err e ->
                            onError (errorToString e)

                -- === UI ===
                "LanguageChanged" ->
                    case decodeValue languageDecoder outsideInfo.data of
                        Ok lang ->
                            tagger <| LanguageChanged lang

                        Err e ->
                            onError (errorToString e)

                "ViewVideos" ->
                    tagger <| ViewVideos

                "FontSelectorOpen" ->
                    case decodeValue (Dec.list Dec.string) outsideInfo.data of
                        Ok fonts ->
                            tagger <| FontSelectorOpen fonts

                        Err e ->
                            onError (errorToString e)

                "Keyboard" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok shortcut ->
                            tagger <| Keyboard shortcut

                        Err e ->
                            onError (errorToString e)

                -- === Misc ===
                "RecvCollabState" ->
                    case decodeValue collabStateDecoder outsideInfo.data of
                        Ok collabState ->
                            tagger <| RecvCollabState collabState

                        Err e ->
                            onError (errorToString e)

                "CollaboratorDisconnected" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok uid ->
                            tagger <| CollaboratorDisconnected uid

                        Err e ->
                            onError (errorToString e)

                _ ->
                    onError <| "Unexpected info from outside: " ++ Debug.toString outsideInfo
        )


encodeAndSend : OutgoingMsg -> Enc.Value -> Cmd msg
encodeAndSend info data =
    let
        tagName =
            unionTypeToString info
    in
    infoForOutside { tag = tagName, data = data }


unionTypeToString : a -> String
unionTypeToString ut =
    ut
        |> Debug.toString
        |> String.words
        |> List.head
        |> Maybe.withDefault (ut |> Debug.toString)



-- DECODERS


cursorPositionDecoder : Decoder CursorPosition
cursorPositionDecoder =
    Dec.map
        (\s ->
            case s of
                "start" ->
                    Start

                "end" ->
                    End

                "other" ->
                    Other

                _ ->
                    Other
        )
        Dec.string


textCursorInfoDecoder : Decoder TextCursorInfo
textCursorInfoDecoder =
    Dec.map3 TextCursorInfo
        (field "selected" Dec.bool)
        (field "position" cursorPositionDecoder)
        (field "text" (tupleDecoder Dec.string Dec.string))


exportSettingsDecoder : Decoder ExportSettings
exportSettingsDecoder =
    let
        formatFromString s =
            case s of
                "json" ->
                    JSON

                "txt" ->
                    TXT

                "docx" ->
                    DOCX

                _ ->
                    JSON

        formatDecoder =
            Dec.map formatFromString Dec.string

        exportStringDecoder =
            Dec.map
                (\s ->
                    case s of
                        "all" ->
                            All

                        "current" ->
                            CurrentSubtree

                        _ ->
                            All
                )
                Dec.string

        exportColumnDecoder =
            Dec.map
                (\i -> ColumnNumber i)
                (field "column" Dec.int)

        exportSelectionDecoder =
            oneOf
                [ exportStringDecoder
                , exportColumnDecoder
                ]

        exportFilepathDecoder =
            Dec.maybe Dec.string
    in
    Dec.map3 ExportSettings
        (field "format" formatDecoder)
        (field "selection" exportSelectionDecoder)
        (field "filepath" exportFilepathDecoder)



-- PORTS


port infoForOutside : OutsideData -> Cmd msg


port infoForElm : (OutsideData -> msg) -> Sub msg
