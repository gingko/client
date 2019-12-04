port module Ports exposing (encodeAndSend, infoForElm, infoForOutside, receiveMsg, sendOut, unionTypeToString)

import Coders exposing (..)
import Json.Decode exposing (decodeValue, errorToString)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Time
import Translation exposing (languageDecoder)
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

        SetChanged changed ->
            dataToSend (bool changed)

        ConfirmCancelCard id origContent ->
            dataToSend (list string [ id, origContent ])

        ColumnNumberChange cols ->
            dataToSend (int cols)

        -- === Database ===
        CommitWithTimestamp ->
            dataToSend null

        SaveToDB ( statusValue, objectsValue ) ->
            dataToSend (list identity [ statusValue, objectsValue ])

        SaveLocal tree ->
            dataToSend (treeToValue tree)

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
                "Commit" ->
                    case decodeValue Json.Decode.int outsideInfo.data of
                        Ok time ->
                            tagger <| Commit time

                        Err e ->
                            onError (errorToString e)

                "GetDataToSave" ->
                    tagger <| GetDataToSave

                "SetHeadRev" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok rev ->
                            tagger <| SetHeadRev rev

                        Err e ->
                            onError (errorToString e)

                "SetLastCommitSaved" ->
                    case decodeValue (Json.Decode.maybe Json.Decode.int) outsideInfo.data of
                        Ok time_ ->
                            tagger <| SetLastCommitSaved (Maybe.map Time.millisToPosix time_)

                        Err e ->
                            onError (errorToString e)

                "SetLastFileSaved" ->
                    case decodeValue (Json.Decode.maybe Json.Decode.float) outsideInfo.data of
                        Ok time_ ->
                            tagger <| SetLastFileSaved (Maybe.map (Time.millisToPosix << round) time_)

                        Err e ->
                            onError (errorToString e)

                "Merge" ->
                    tagger <| Merge outsideInfo.data

                -- === DOM ===
                "DragStarted" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok dragId ->
                            tagger <| DragStarted dragId

                        Err e ->
                            onError (errorToString e)

                "FieldChanged" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
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
                    case decodeValue (tupleDecoder Json.Decode.string Json.Decode.int) outsideInfo.data of
                        Ok ( cardId, checkboxNumber ) ->
                            tagger <| CheckboxClicked cardId checkboxNumber

                        Err e ->
                            onError (errorToString e)

                -- === UI ===
                "SetLanguage" ->
                    case decodeValue languageDecoder outsideInfo.data of
                        Ok lang ->
                            tagger <| SetLanguage lang

                        Err e ->
                            onError (errorToString e)

                "ViewVideos" ->
                    tagger <| ViewVideos

                "FontSelectorOpen" ->
                    case decodeValue (Json.Decode.list Json.Decode.string) outsideInfo.data of
                        Ok fonts ->
                            tagger <| FontSelectorOpen fonts

                        Err e ->
                            onError (errorToString e)

                "Keyboard" ->
                    case decodeValue Json.Decode.string outsideInfo.data of
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
                    case decodeValue Json.Decode.string outsideInfo.data of
                        Ok uid ->
                            tagger <| CollaboratorDisconnected uid

                        Err e ->
                            onError (errorToString e)

                _ ->
                    onError <| "Unexpected info from outside: " ++ Debug.toString outsideInfo
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
        |> Debug.toString
        |> String.words
        |> List.head
        |> Maybe.withDefault (ut |> Debug.toString)


port infoForOutside : OutsideData -> Cmd msg


port infoForElm : (OutsideData -> msg) -> Sub msg
