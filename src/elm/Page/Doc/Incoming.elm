port module Page.Doc.Incoming exposing (Msg(..), subscribe)

import Coders exposing (..)
import Json.Decode as Dec exposing (Decoder, decodeValue, errorToString, field, oneOf)
import Time
import Translation exposing (languageDecoder)
import Types exposing (Children(..), CollabState, CursorPosition(..), OutsideData, TextCursorInfo, Tree)


type
    Msg
    -- === Dialogs, Menus, Window State ===
    = CancelCardConfirmed
      -- === Database ===
    | DataSaved Dec.Value
    | DataReceived Dec.Value
    | NotFound
    | LocalStoreLoaded Dec.Value
    | GetDataToSave
    | SavedLocally (Maybe Time.Posix)
    | SavedRemotely Time.Posix
      -- === Metadata ===
    | MetadataSynced Dec.Value
    | MetadataSaved Dec.Value
    | MetadataSaveError
      -- === DOM ===
    | DragStarted String
    | FullscreenChanged Bool
    | Paste Tree
    | PasteInto Tree
    | FieldChanged String
    | TextCursor TextCursorInfo
    | CheckboxClicked String Int
      -- === UI ===
    | ViewVideos
    | FontSelectorOpen (List String)
    | Keyboard String
      -- === Misc ===
    | RecvCollabState CollabState
    | CollaboratorDisconnected String



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



-- SUBSCRIPTION HELPER


subscribe : (Msg -> msg) -> (String -> msg) -> Sub msg
subscribe tagger onError =
    docMsgs
        (\outsideInfo ->
            case outsideInfo.tag of
                -- === Dialogs, Menus, Window State ===
                "CancelCardConfirmed" ->
                    tagger <| CancelCardConfirmed

                -- === Database ===
                "DataSaved" ->
                    tagger <| DataSaved outsideInfo.data

                "DataReceived" ->
                    tagger <| DataReceived outsideInfo.data

                "NotFound" ->
                    tagger <| NotFound

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
                    case decodeValue Dec.int outsideInfo.data of
                        Ok time ->
                            tagger <| SavedRemotely (Time.millisToPosix time)

                        Err e ->
                            onError (errorToString e)

                -- === DOM ===
                "DragStarted" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok dragId ->
                            tagger <| DragStarted dragId

                        Err e ->
                            onError (errorToString e)

                "FullscreenChanged" ->
                    case decodeValue Dec.bool outsideInfo.data of
                        Ok isFullscreen ->
                            tagger <| FullscreenChanged isFullscreen

                        Err e ->
                            onError (errorToString e)

                "Paste" ->
                    case decodeValue treeOrString outsideInfo.data of
                        Ok tree ->
                            tagger <| Paste tree

                        Err e ->
                            onError (errorToString e)

                "PasteInto" ->
                    case decodeValue treeOrString outsideInfo.data of
                        Ok tree ->
                            tagger <| PasteInto tree

                        Err e ->
                            onError (errorToString e)

                "FieldChanged" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok str ->
                            tagger <| FieldChanged str

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



-- PORT


port docMsgs : (OutsideData -> msg) -> Sub msg
