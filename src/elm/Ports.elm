port module Ports exposing (ExportFormat(..), ExportSelection(..), ExportSettings, IncomingMsg(..), OutgoingMsg(..), infoForElm, infoForOutside, receiveMsg, sendOut)

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


type OutgoingMsg
    = StoreSession (Maybe String)
      -- === Dialogs, Menus, Window State ===
    | Alert String
    | SetChanged Bool
    | ConfirmCancelCard String String
    | ColumnNumberChange Int
      -- === Database ===
    | InitDocument String
    | LoadDocument String
    | GetDocumentList String
    | RequestDelete String
    | NoDataToSave
    | SaveData Enc.Value Enc.Value
    | SaveImportedData Enc.Value
    | Push
    | Pull
      -- === File System ===
    | ExportDOCX String (Maybe String)
    | ExportJSON Tree (Maybe String)
    | ExportTXT Bool Tree (Maybe String)
    | ExportTXTColumn Int Tree (Maybe String)
      -- === DOM ===
    | ActivateCards ( String, Int, List (List String) ) Bool
    | DragStart Enc.Value
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
    | GetDataToSave
    | SavedLocally (Maybe Time.Posix)
    | SavedRemotely (Maybe Time.Posix)
      -- === Home Page ===
    | ImportComplete
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
    case info of
        StoreSession str_ ->
            dataToSend "StoreSession" (maybe string str_)

        -- === Dialogs, Menus, Window State ===
        Alert str ->
            dataToSend "Alert" (string str)

        SetChanged changed ->
            dataToSend "SetChanged" (bool changed)

        ConfirmCancelCard id origContent ->
            dataToSend "ConfirmCancelCard" (list string [ id, origContent ])

        ColumnNumberChange cols ->
            dataToSend "ColumnNumberChange" (int cols)

        -- === Database ===
        SaveData data metadata ->
            dataToSend "SaveData" (object [ ( "data", data ), ( "metadata", metadata ) ])

        SaveImportedData data ->
            dataToSend "SaveImportedData" data

        InitDocument dbName ->
            dataToSend "InitDocument" (string dbName)

        LoadDocument dbName ->
            dataToSend "LoadDocument" (string dbName)

        GetDocumentList dbName ->
            dataToSend "GetDocumentList" (string dbName)

        RequestDelete dbName ->
            dataToSend "RequestDelete" (string dbName)

        NoDataToSave ->
            dataToSend "NoDataToSave" null

        Push ->
            dataToSend "Push" null

        Pull ->
            dataToSend "Pull" null

        -- === File System ===
        ExportDOCX str path_ ->
            dataToSend "ExportDOCX"
                (object
                    [ ( "data", string str )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        ExportJSON tree path_ ->
            dataToSend "ExportJSON"
                (object
                    [ ( "data", treeToJSON tree )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        ExportTXT withRoot tree path_ ->
            dataToSend "ExportTXT"
                (object
                    [ ( "data", treeToMarkdown withRoot tree )
                    , ( "filepath", maybe string path_ )
                    ]
                )

        ExportTXTColumn col tree path_ ->
            dataToSend "ExportTXT"
                (object
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
                )

        -- === DOM ===
        ActivateCards ( cardId, col, lastActives ) instant ->
            let
                listListStringToValue lls =
                    list (list string) lls
            in
            dataToSend "ActivateCards"
                (object
                    [ ( "cardId", string cardId )
                    , ( "column", int col )
                    , ( "lastActives", listListStringToValue lastActives )
                    , ( "instant", bool instant )
                    ]
                )

        DragStart event ->
            dataToSend "DragStart" event

        FlashCurrentSubtree ->
            dataToSend "FlashCurrentSubtree" null

        TextSurround id str ->
            dataToSend "TextSurround" (list string [ id, str ])

        SetCursorPosition pos ->
            dataToSend "SetCursorPosition" (int pos)

        -- === UI ===
        SaveMetadata metadata ->
            dataToSend "SaveMetadata" metadata

        UpdateCommits ( objectsValue, head_ ) ->
            let
                headToValue mbs =
                    case mbs of
                        Just str ->
                            string str

                        Nothing ->
                            null
            in
            dataToSend "UpdateCommits" (tupleToValue identity ( objectsValue, headToValue head_ ))

        SetVideoModal isOpen ->
            dataToSend "SetVideoModal" (bool isOpen)

        SetFonts fontSettings ->
            dataToSend "SetFonts" (fontSettingsEncoder fontSettings)

        SetShortcutTray isOpen ->
            dataToSend "SetShortcutTray" (bool isOpen)

        -- === Misc ===
        SocketSend collabState ->
            dataToSend "SocketSend" (collabStateToValue collabState)

        ConsoleLogRequested err ->
            dataToSend "ConsoleLogRequested" (string err)


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

                "ImportComplete" ->
                    tagger <| ImportComplete

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
                    onError <| "Unexpected info from outside: " ++ outsideInfo.tag
        )


dataToSend : String -> Enc.Value -> Cmd msg
dataToSend tagName data =
    infoForOutside { tag = tagName, data = data }



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
