port module Outgoing exposing (Msg(..), infoForOutside, send)

import Coders exposing (..)
import Doc.Fonts as Fonts
import Doc.TreeUtils exposing (getColumn)
import Json.Encode as Enc exposing (..)
import Json.Encode.Extra exposing (maybe)
import Translation exposing (Language, langToString)
import Types exposing (CollabState, CursorPosition(..), OutsideData, TextCursorInfo, Tree)



-- TYPES


type Msg
    = StoreUser Enc.Value
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
    | SaveData Enc.Value
    | SaveImportedData Enc.Value
    | Push
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
    | SetLanguage Language
    | SetFonts Fonts.Settings
    | SetShortcutTray Bool
      -- === Misc ===
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

        SetChanged changed ->
            dataToSend "SetChanged" (bool changed)

        ConfirmCancelCard id origContent ->
            dataToSend "ConfirmCancelCard" (list string [ id, origContent ])

        ColumnNumberChange cols ->
            dataToSend "ColumnNumberChange" (int cols)

        -- === Database ===
        SaveData data ->
            dataToSend "SaveData" data

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

        SetLanguage lang ->
            dataToSend "SetLanguage" (lang |> langToString |> string)

        SetFonts fontSettings ->
            dataToSend "SetFonts" (fontSettingsEncoder fontSettings)

        SetShortcutTray isOpen ->
            dataToSend "SetShortcutTray" (bool isOpen)

        -- === Misc ===
        SocketSend collabState ->
            dataToSend "SocketSend" (collabStateToValue collabState)

        ConsoleLogRequested err ->
            dataToSend "ConsoleLogRequested" (string err)


dataToSend : String -> Enc.Value -> Cmd msg
dataToSend tagName data =
    infoForOutside { tag = tagName, data = data }



-- PORTS


port infoForOutside : OutsideData -> Cmd msg
