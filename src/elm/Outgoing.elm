port module Outgoing exposing (Msg(..), infoForOutside, send)

import Coders exposing (..)
import Doc.Fonts as Fonts
import Doc.TreeUtils exposing (ScrollPosition, getColumn, scrollPositionToValue)
import Json.Encode as Enc exposing (..)
import Json.Encode.Extra exposing (maybe)
import Page.Doc.Theme as Theme exposing (Theme)
import Translation exposing (Language, langToString)
import Types exposing (CollabState, CursorPosition(..), OutsideData, TextCursorInfo, Tree)



-- TYPES


type Msg
    = StoreUser Enc.Value
      -- === Dialogs, Menus, Window State ===
    | Alert String
    | SetDirty Bool
    | ConfirmCancelCard String String
    | ColumnNumberChange Int
      -- === Database ===
    | InitDocument String
    | LoadDocument String
    | GetDocumentList String
    | RequestDelete String
    | NoDataToSave
    | SaveData Enc.Value
    | PullData
    | SaveImportedData Enc.Value
      -- === DOM ===
    | ScrollCards (List String) (List ( Int, ScrollPosition )) Int Bool
    | ScrollFullscreenCards String
    | DragStart Enc.Value
    | CopyCurrentSubtree Enc.Value
    | TextSurround String String
    | SetCursorPosition Int
    | SetFullscreen Bool
      -- === UI ===
    | UpdateCommits ( Enc.Value, Maybe String )
    | SetVideoModal Bool
    | SetLanguage Language
    | SaveThemeSetting Theme
    | SetFonts Fonts.Settings
    | SetShortcutTray Bool
      -- === Misc ===
    | EmptyMessageShown
    | TriggerMailto
    | SocketSend CollabState
    | ConsoleLogRequested String



-- HELPERS


send : Msg -> Cmd msg
send info =
    case info of
        StoreUser user ->
            dataToSend "StoreUser" user

        -- === Dialogs, Menus, Window State ===
        Alert str ->
            dataToSend "Alert" (string str)

        SetDirty changed ->
            dataToSend "SetDirty" (bool changed)

        ConfirmCancelCard id origContent ->
            dataToSend "ConfirmCancelCard" (list string [ id, origContent ])

        ColumnNumberChange cols ->
            dataToSend "ColumnNumberChange" (int cols)

        -- === Database ===
        SaveData data ->
            dataToSend "SaveData" data

        PullData ->
            dataToSend "PullData" null

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

        -- === DOM ===
        ScrollCards lastActives listScrollPositions colIdx instant ->
            dataToSend "ScrollCards"
                (object
                    [ ( "columns"
                      , listScrollPositions
                            |> List.map
                                (\( idx, sp ) ->
                                    Enc.object
                                        [ ( "columnIdx", Enc.int idx )
                                        , ( "scrollData", scrollPositionToValue sp )
                                        ]
                                )
                            |> Enc.list identity
                      )
                    , ( "lastActives", Enc.list string lastActives )
                    , ( "columnIdx", int colIdx )
                    , ( "instant", bool instant )
                    ]
                )

        ScrollFullscreenCards cardId ->
            dataToSend "ScrollFullscreenCards" (string cardId)

        DragStart event ->
            dataToSend "DragStart" event

        CopyCurrentSubtree treeJSON ->
            dataToSend "CopyCurrentSubtree" treeJSON

        TextSurround id str ->
            dataToSend "TextSurround" (list string [ id, str ])

        SetCursorPosition pos ->
            dataToSend "SetCursorPosition" (int pos)

        SetFullscreen shouldFullscreen ->
            dataToSend "SetFullscreen" (bool shouldFullscreen)

        -- === UI ===
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

        SetLanguage lang ->
            dataToSend "SetLanguage" (lang |> langToString |> string)

        SaveThemeSetting newTheme ->
            dataToSend "SaveThemeSetting" (Theme.toValue newTheme)

        SetFonts fontSettings ->
            dataToSend "SetFonts" (fontSettingsEncoder fontSettings)

        SetShortcutTray isOpen ->
            dataToSend "SetShortcutTray" (bool isOpen)

        -- === Misc ===
        EmptyMessageShown ->
            dataToSend "EmptyMessageShown" null

        TriggerMailto ->
            dataToSend "TriggerMailto" null

        SocketSend collabState ->
            dataToSend "SocketSend" (collabStateToValue collabState)

        ConsoleLogRequested err ->
            dataToSend "ConsoleLogRequested" (string err)


dataToSend : String -> Enc.Value -> Cmd msg
dataToSend tagName data =
    infoForOutside { tag = tagName, data = data }



-- PORTS


port infoForOutside : OutsideData -> Cmd msg
