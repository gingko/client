port module Outgoing exposing (Msg(..), infoForOutside, send)

import Coders exposing (..)
import Doc.Fonts as Fonts
import Doc.TreeUtils exposing (ScrollPosition, scrollPositionToValue)
import Json.Encode as Enc exposing (..)
import Page.Doc.Theme as Theme exposing (Theme)
import Types exposing (Collaborator, CursorPosition(..), DropId, OutsideData, TextCursorInfo, Tree)



-- TYPES


type Msg
    = StoreUser Enc.Value
    | LogoutUser
    | SaveUserSetting ( String, Enc.Value )
      -- === Dialogs, Menus, Window State ===
    | Alert String
    | SetDirty Bool
    | DragDone
    | ConfirmCancelCard String String String
      -- === Database ===
    | InitDocument String
    | LoadDocument String
    | CopyDocument String
    | GetDocumentList
    | RequestDelete String (Maybe String)
    | NoDataToSave
    | RenameDocument String
    | SaveCardBased Enc.Value
    | SaveImportedTree ( String, String )
    | SaveCardBasedMigration Enc.Value
    | PushDeltas Enc.Value
    | CommitData Enc.Value
    | PullData
    | SaveImportedData Enc.Value
    | SaveBulkImportedData Enc.Value
      -- === AI ===
    | GenerateChildren { prompt : String, id : String }
    | GenerateBelow { prompt : String, id : String }
      -- === Collaboration ===
    | AddCollabRequest String String
    | RemoveCollabRequest String String
    | SendCollabState Enc.Value
      -- === Desktop ===
    | SaveToFile String String
    | ExportToFile String String
      -- === DOM ===
    | ScrollCards (List String) (List ( Int, ScrollPosition )) Int Bool
    | ScrollFullscreenCards String
    | DragStart Enc.Value
    | CopyCurrentSubtree Enc.Value
    | CopyToClipboard String String
    | SelectAll String
    | FlashPrice
    | TextSurround String String
    | SetField String String
    | SetCursorPosition Int
    | SetFullscreen Bool
    | PositionTourStep Int String
      -- === UI ===
    | UpdateCommits ( Enc.Value, Maybe String )
    | HistorySlider Bool Int
    | SetSidebarState Bool
    | SaveThemeSetting Theme
    | RequestFullscreen
    | Print
    | SetFonts Fonts.Settings
      -- === Misc ===
    | IntegrationTestEvent String
    | EmptyMessageShown
    | ShowWidget
    | CheckoutButtonClicked Enc.Value
    | ConsoleLogRequested String



-- HELPERS


send : Msg -> Cmd msg
send info =
    case info of
        StoreUser user ->
            dataToSend "StoreUser" user

        LogoutUser ->
            dataToSend "LogoutUser" null

        SaveUserSetting ( key, val ) ->
            dataToSend "SaveUserSetting" (list identity [ string key, val ])

        -- === Dialogs, Menus, Window State ===
        Alert str ->
            dataToSend "Alert" (string str)

        SetDirty changed ->
            dataToSend "SetDirty" (bool changed)

        DragDone ->
            dataToSend "DragDone" null

        ConfirmCancelCard id origContent confirmText ->
            dataToSend "ConfirmCancelCard" (list string [ id, origContent, confirmText ])

        -- === Database ===
        RenameDocument newDocName ->
            dataToSend "RenameDocument" (string newDocName)

        SaveCardBased data ->
            dataToSend "SaveCardBased" data

        SaveImportedTree ( docId, name ) ->
            dataToSend "SaveCardBasedTree" (tupleToValue string ( docId, name ))

        SaveCardBasedMigration data ->
            dataToSend "SaveCardBasedMigration" data

        PushDeltas data ->
            dataToSend "PushDeltas" data

        CommitData data ->
            dataToSend "CommitData" data

        PullData ->
            dataToSend "PullData" null

        SaveImportedData data ->
            dataToSend "SaveImportedData" data

        SaveBulkImportedData data ->
            dataToSend "SaveBulkImportedData" data

        InitDocument dbName ->
            dataToSend "InitDocument" (string dbName)

        LoadDocument dbName ->
            dataToSend "LoadDocument" (string dbName)

        CopyDocument dbName ->
            dataToSend "CopyDocument" (string dbName)

        GetDocumentList ->
            dataToSend "GetDocumentList" null

        RequestDelete dbName docName_ ->
            dataToSend "RequestDelete" (tupleToValue string ( dbName, docName_ |> Maybe.withDefault "Untitled" ))

        NoDataToSave ->
            dataToSend "NoDataToSave" null

        -- === AI ===
        GenerateChildren { prompt, id } ->
            dataToSend "GenerateChildren" (tupleToValue string ( id, prompt ))

        GenerateBelow { prompt, id } ->
            dataToSend "GenerateBelow" (tupleToValue string ( id, prompt ))

        -- === Collaboration ===
        AddCollabRequest treeId collabEmail ->
            dataToSend "AddCollabRequest" (tupleToValue string ( treeId, collabEmail ))

        RemoveCollabRequest treeId collabEmail ->
            dataToSend "RemoveCollabRequest" (tupleToValue string ( treeId, collabEmail ))

        SendCollabState collabState ->
            dataToSend "SendCollabState" collabState

        -- === Desktop ===
        SaveToFile filename str ->
            dataToSend "SaveToFile" (tupleToValue string ( filename, str ))

        ExportToFile format str ->
            dataToSend "ExportToFile" (tupleToValue string ( format, str ))

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

        CopyToClipboard what flash ->
            dataToSend "CopyToClipboard" (object [ ( "content", string what ), ( "element", string flash ) ])

        SelectAll what ->
            dataToSend "SelectAll" (string what)

        FlashPrice ->
            dataToSend "FlashPrice" null

        TextSurround id str ->
            dataToSend "TextSurround" (list string [ id, str ])

        SetField id str ->
            dataToSend "SetField" (list string [ id, str ])

        SetCursorPosition pos ->
            dataToSend "SetCursorPosition" (int pos)

        SetFullscreen shouldFullscreen ->
            dataToSend "SetFullscreen" (bool shouldFullscreen)

        PositionTourStep step elId ->
            dataToSend "PositionTourStep" (tupleToValue identity ( int step, string elId ))

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

        HistorySlider firstOpen delta ->
            dataToSend "HistorySlider" (tupleToValue identity ( bool firstOpen, int delta ))

        SetSidebarState isOpen ->
            dataToSend "SetSidebarState" (bool isOpen)

        SaveThemeSetting newTheme ->
            dataToSend "SaveThemeSetting" (Theme.toValue newTheme)

        RequestFullscreen ->
            dataToSend "RequestFullscreen" null

        Print ->
            dataToSend "Print" null

        SetFonts fontSettings ->
            dataToSend "SetFonts" (fontSettingsEncoder fontSettings)

        -- === Misc ===
        IntegrationTestEvent eventData ->
            dataToSend "IntegrationTestEvent" (string eventData)

        EmptyMessageShown ->
            dataToSend "EmptyMessageShown" null

        ShowWidget ->
            dataToSend "ShowWidget" null

        CheckoutButtonClicked checkoutData ->
            dataToSend "CheckoutButtonClicked" checkoutData

        ConsoleLogRequested err ->
            dataToSend "ConsoleLogRequested" (string err)


dataToSend : String -> Enc.Value -> Cmd msg
dataToSend tagName data =
    infoForOutside { tag = tagName, data = data }



-- PORTS


port infoForOutside : OutsideData -> Cmd msg
