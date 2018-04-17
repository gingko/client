port module Ports exposing (..)


import Types exposing (..)
import Coders exposing (..)
import TreeUtils exposing (getColumn)
import Json.Encode exposing (..)
import Json.Decode exposing (decodeValue)


sendOut : OutgoingMsg -> Cmd msg
sendOut info =
  let
    dataToSend = encodeAndSend info
  in
  case info of
    SaveAnd actionName filepath_ (statusValue, objectsValue) ->
      dataToSend
        ( object
          [ ( "action", string actionName )
          , ( "filepath", maybeToValue string filepath_ )
          , ( "document", list [ statusValue, objectsValue ] )
          ]
        )

    Alert str ->
      infoForOutside
        { tag = "Alert"
        , data = string str
        }

    ConfirmClose actionName filepath_ (statusValue, objectsValue) ->
      dataToSend
        ( object
            [ ( "action", string actionName  )
            , ( "filepath", maybeToValue string filepath_ )
            , ( "document", list [ statusValue, objectsValue ] )
            ]
        )

    ConfirmExit filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    Exit ->
      dataToSend null

    ActivateCards (cardId, col, cardIds) ->
      let
        listListStringToValue lls =
          lls
            |> List.map (List.map string)
            |> List.map list
            |> list
      in
      infoForOutside
        { tag = "ActivateCards"
        , data = tripleToValue string int listListStringToValue ( cardId, col, cardIds )
        }

    TextSurround id str ->
      infoForOutside
        { tag = "TextSurround"
        , data = list [ string id, string str ]
        }

    ConfirmCancelCard id origContent ->
      infoForOutside
        { tag = "ConfirmCancelCard"
        , data = list [ string id, string origContent ]
        }

    ColumnNumberChange cols ->
      infoForOutside
        { tag = "ColumnNumberChange"
        , data = int cols
        }

    OpenDialog filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    Save filepath_ ->
      dataToSend ( maybeToValue string filepath_ )

    ExportJSON tree ->
      infoForOutside
        { tag = "ExportJSON"
        , data = treeToJSON tree
        }

    ExportTXT withRoot tree ->
      infoForOutside
        { tag = "ExportTXT"
        , data = treeToMarkdown withRoot tree
        }

    ExportTXTColumn col tree ->
      infoForOutside
        { tag = "ExportTXT"
        , data =
            tree
              |> getColumn col
              |> Maybe.withDefault [[]]
              |> List.concat
              |> List.map .content
              |> String.join "\n\n"
              |> string
        }

    Push ->
      tagOnly "Push"

    Pull ->
      tagOnly "Pull"

    SaveToDB ( statusValue, objectsValue ) ->
      dataToSend ( list [ statusValue, objectsValue ] )

    SaveLocal tree ->
      infoForOutside
        { tag = "SaveLocal"
        , data = treeToValue tree
        }

    UpdateCommits ( objectsValue, head_ ) ->
      let
        headToValue mbs =
          case mbs of
            Just str -> string str
            Nothing -> null
      in
      infoForOutside
        { tag = "UpdateCommits"
        , data = tupleToValue identity headToValue ( objectsValue, head_ )
        }

    SetVideoModal isOpen ->
      infoForOutside
        { tag = "SetVideoModal"
        , data = bool isOpen
        }

    SetShortcutTray isOpen ->
      infoForOutside
        { tag = "SetShortcutTray"
        , data = bool isOpen
        }

    SocketSend collabState ->
      infoForOutside
        { tag = "SocketSend"
        , data = collabStateToValue collabState
        }

    ConsoleLogRequested err ->
      infoForOutside
        { tag = "ConsoleLogRequested"
        , data = string err
        }


receiveMsg : (IncomingMsg -> msg) -> (String -> msg) -> Sub msg
receiveMsg tagger onError =
  infoForElm
    (\outsideInfo ->
        case outsideInfo.tag of
          "New" ->
            tagger <| New

          "FieldChanged" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok newField ->
                tagger <| FieldChanged newField

              Err e ->
                onError e

          "IntentNew" ->
            tagger <| IntentNew

          "IntentOpen" ->
            tagger <| IntentOpen

          "IntentImport" ->
            tagger <| IntentImport

          "IntentSave" ->
            tagger <| IntentSave

          "IntentSaveAs" ->
            tagger <| IntentSaveAs

          "IntentExport" ->
            case decodeValue exportSettingsDecoder outsideInfo.data of
              Ok exportSettings ->
                tagger <| IntentExport exportSettings

              Err e ->
                onError e

          "IntentExit" ->
            tagger <| IntentExit

          "CancelCardConfirmed" ->
            tagger <| CancelCardConfirmed

          "Open" ->
            case decodeValue ( tripleDecoder Json.Decode.string Json.Decode.value (Json.Decode.maybe Json.Decode.string) ) outsideInfo.data of
              Ok ( filepath, json, lastActive_ ) ->
                tagger <| Open (filepath, json, lastActive_ |> Maybe.withDefault "1" )

              Err e ->
                onError e

          "Merge" ->
            tagger <| Merge outsideInfo.data

          "ImportJSON" ->
            tagger <| ImportJSON outsideInfo.data

          "CheckoutCommit" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok commitSha ->
                tagger <| CheckoutCommit commitSha

              Err e ->
                onError e

          "SetHeadRev" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok rev ->
                tagger <| SetHeadRev rev

              Err e ->
                onError e

          "FileState" ->
            let decoder = tupleDecoder (Json.Decode.maybe Json.Decode.string) Json.Decode.bool in
            case decodeValue decoder outsideInfo.data of
              Ok (filepath_, changed) ->
                tagger <| FileState filepath_ changed

              Err e ->
                onError e

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

          "ViewVideos" ->
            tagger <| ViewVideos

          "Keyboard" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok shortcut ->
                tagger <| Keyboard shortcut

              Err e ->
                onError e

          _ ->
            onError <| "Unexpected info from outside: " ++ toString outsideInfo
    )


tagOnly : String -> Cmd msg
tagOnly tag =
  infoForOutside { tag = tag, data = null }


encodeAndSend : OutgoingMsg -> Json.Encode.Value -> Cmd msg
encodeAndSend info data =
  let
    tagName = unionTypeToString info
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
