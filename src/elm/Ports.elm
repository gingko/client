port module Ports exposing (..)

import Coders exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode exposing (..)
import TreeUtils exposing (getColumn)
import Types exposing (..)


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

        SaveAndClose toSave_ ->
            let
                toSaveData =
                    case toSave_ of
                        Nothing ->
                            null

                        Just ( statusValue, objectsValue ) ->
                            list [ statusValue, objectsValue ]
            in
            dataToSend toSaveData

        ConfirmCancelCard id origContent ->
            dataToSend (list [ string id, string origContent ])

        ColumnNumberChange cols ->
            dataToSend (int cols)

        -- === Database ===
        SaveToDB ( statusValue, objectsValue ) ->
            dataToSend (list [ statusValue, objectsValue ])

        SaveLocal tree ->
            dataToSend (treeToValue tree)

        Push ->
            dataToSend null

        Pull ->
            dataToSend null

        -- === File System ===
        ExportDOCX str ->
            dataToSend (string str)

        ExportJSON tree ->
            dataToSend (treeToJSON tree)

        ExportTXT withRoot tree ->
            dataToSend (treeToMarkdown withRoot tree)

        -- ExportTXTColumn is handled by 'ExportTXT' in JS
        -- So we use the "ExportTXT" tag here, instead of `dataToSend`
        ExportTXTColumn col tree ->
            infoForOutside
                { tag = "ExportTXT"
                , data =
                    tree
                        |> getColumn col
                        |> Maybe.withDefault [ [] ]
                        |> List.concat
                        |> List.map .content
                        |> String.join "\n\n"
                        |> string
                }

        -- === DOM ===
        ActivateCards ( cardId, col, lastActives ) ->
            let
                listListStringToValue lls =
                    lls
                        |> List.map (List.map string)
                        |> List.map list
                        |> list
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
            dataToSend (list [ string id, string str ])

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
            dataToSend (tupleToValue identity headToValue ( objectsValue, head_ ))

        SetVideoModal isOpen ->
            dataToSend (bool isOpen)

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
                "IntentExit" ->
                    tagger <| IntentExit

                "IntentExport" ->
                    case decodeValue exportSettingsDecoder outsideInfo.data of
                        Ok exportSettings ->
                            tagger <| IntentExport exportSettings

                        Err e ->
                            onError e

                "CancelCardConfirmed" ->
                    tagger <| CancelCardConfirmed

                -- === Database ===
                "SetHeadRev" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok rev ->
                            tagger <| SetHeadRev rev

                        Err e ->
                            onError e

                "Merge" ->
                    tagger <| Merge outsideInfo.data

                -- === DOM ===
                "FieldChanged" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok newField ->
                            tagger <| FieldChanged newField

                        Err e ->
                            onError e

                "TextSelected" ->
                    case decodeValue Json.Decode.bool outsideInfo.data of
                        Ok newBool ->
                            tagger <| TextSelected newBool

                        Err e ->
                            onError e

                -- === UI ===
                "CheckoutCommit" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok commitSha ->
                            tagger <| CheckoutCommit commitSha

                        Err e ->
                            onError e

                "ViewVideos" ->
                    tagger <| ViewVideos

                "Keyboard" ->
                    case decodeValue (tupleDecoder Json.Decode.string Json.Decode.int) outsideInfo.data of
                        Ok ( shortcut, timestamp ) ->
                            tagger <| Keyboard shortcut timestamp

                        Err e ->
                            onError e

                -- === Misc ===
                "RecvCollabState" ->
                    case decodeValue collabStateDecoder outsideInfo.data of
                        Ok collabState ->
                            tagger <| RecvCollabState collabState

                        Err e ->
                            onError e

                "CollaboratorDisconnected" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok uid ->
                            tagger <| CollaboratorDisconnected uid

                        Err e ->
                            onError e

                _ ->
                    onError <| "Unexpected info from outside: " ++ toString outsideInfo
        )


encodeAndSend : OutgoingMsg -> Json.Encode.Value -> Cmd msg
encodeAndSend info data =
    let
        tagName =
            unionTypeToString info
    in
    infoForOutside { tag = tagName, data = data }


unionTypeToString : a -> String
unionTypeToString ut =
    ut
        |> toString
        |> String.words
        |> List.head
        |> Maybe.withDefault (ut |> toString)


port infoForOutside : OutsideData -> Cmd msg


port infoForElm : (OutsideData -> msg) -> Sub msg
