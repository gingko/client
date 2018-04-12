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

    ConfirmCancel id origContent ->
      infoForOutside
        { tag = "ConfirmCancel"
        , data = list [ string id, string origContent ]
        }

    ColumnNumberChange cols ->
      infoForOutside
        { tag = "ColumnNumberChange"
        , data = int cols
        }

    New str_ ->
      infoForOutside
        { tag = "New"
        , data =
            case str_ of
              Just str -> string str
              Nothing -> null
        }

    Open filepath_ ->
      infoForOutside
        { tag = "Open"
        , data =
            case filepath_ of
              Just filepath -> string filepath
              Nothing -> null
        }

    Save filepath ->
      infoForOutside
        { tag = "Save"
        , data = string filepath
        }

    SaveAs ->
      tagOnly "SaveAs"

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

    SaveObjects ( statusValue, objectsValue ) ->
      infoForOutside
        { tag = "SaveObjects"
        , data = list [ statusValue, objectsValue ]
        }

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

    SetSaved filepath ->
      infoForOutside
        { tag = "SetSaved"
        , data = string filepath
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

    SetChanged ->
      tagOnly "SetChanged"


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
          "FieldChanged" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok newField ->
                tagger <| FieldChanged newField

              Err e ->
                onError e

          "CancelCardConfirmed" ->
            tagger <| CancelCardConfirmed

          "Reset" ->
            tagger <| Reset

          "Load" ->
            case decodeValue ( tripleDecoder Json.Decode.string Json.Decode.value (Json.Decode.maybe Json.Decode.string) ) outsideInfo.data of
              Ok ( filepath, json, lastActive_ ) ->
                tagger <| Load (filepath, json, lastActive_ |> Maybe.withDefault "1" )

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

          "Changed" ->
            tagger <| Changed

          "Saved" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok filepath ->
                tagger <| Saved filepath

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

          "DoExportJSON" ->
            tagger <| DoExportJSON

          "DoExportTXT" ->
            case decodeValue Json.Decode.int outsideInfo.data of
              Ok col ->
                tagger <| DoExportTXTColumn col

              Err e ->
                tagger <| DoExportTXT

          "DoExportTXTCurrent" ->
              tagger <| DoExportTXTCurrent

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
