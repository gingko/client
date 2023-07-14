port module Page.Doc.Incoming exposing (Msg(..), subscribe)

import Coders exposing (..)
import File exposing (File)
import Json.Decode as Dec exposing (Decoder, decodeValue, errorToString, field)
import Types exposing (Children(..), CollabState, CursorPosition(..), OutsideData, TextCursorInfo, Tree)


type
    Msg
    -- === Dialogs, Menus, Window State ===
    = CancelCardConfirmed
      -- === DOM ===
    | InitialActivation
    | DragStarted String
    | DragExternalStarted
    | DropExternal String
    | Paste Tree
    | PasteInto Tree
    | FieldChanged String
    | AutoSaveRequested
    | FullscreenCardFocused String String
    | TextCursor TextCursorInfo
    | ClickedOutsideCard
    | CheckboxClicked String Int
      -- === UI ===
    | Keyboard String
      -- === Misc ===
    | WillPrint
    | RecvCollabState CollabState
    | CollaboratorDisconnected String
      -- === TESTING ===
    | TestTextImportLoaded (List File)



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

                -- === DOM ===
                "InitialActivation" ->
                    tagger <| InitialActivation

                "DragStarted" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok dragId ->
                            tagger <| DragStarted dragId

                        Err e ->
                            onError (errorToString e)

                "DragExternalStarted" ->
                    tagger <| DragExternalStarted

                "DropExternal" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok dropText ->
                            tagger <| DropExternal dropText

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

                "AutoSaveRequested" ->
                    tagger <| AutoSaveRequested

                "FullscreenCardFocused" ->
                    case decodeValue (tupleDecoder Dec.string Dec.string) outsideInfo.data of
                        Ok ( cardId, fieldId ) ->
                            tagger <| FullscreenCardFocused cardId fieldId

                        Err e ->
                            onError (errorToString e)

                "TextCursor" ->
                    case decodeValue textCursorInfoDecoder outsideInfo.data of
                        Ok textCursorInfo ->
                            tagger <| TextCursor textCursorInfo

                        Err e ->
                            onError (errorToString e)

                "ClickedOutsideCard" ->
                    tagger <| ClickedOutsideCard

                "CheckboxClicked" ->
                    case decodeValue (tupleDecoder Dec.string Dec.int) outsideInfo.data of
                        Ok ( cardId, checkboxNumber ) ->
                            tagger <| CheckboxClicked cardId checkboxNumber

                        Err e ->
                            onError (errorToString e)

                -- === UI ===
                "Keyboard" ->
                    case decodeValue Dec.string outsideInfo.data of
                        Ok shortcut ->
                            tagger <| Keyboard shortcut

                        Err e ->
                            onError (errorToString e)

                "WillPrint" ->
                    tagger <| WillPrint

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

                "TestTextImportLoaded" ->
                    case decodeValue (Dec.list File.decoder) outsideInfo.data of
                        Ok files ->
                            tagger <| TestTextImportLoaded files

                        Err e ->
                            onError (errorToString e)

                _ ->
                    onError <| "Unexpected info from outside: " ++ outsideInfo.tag
        )



-- PORT


port docMsgs : (OutsideData -> msg) -> Sub msg
