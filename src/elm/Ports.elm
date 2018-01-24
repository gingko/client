port module Ports exposing (..)


import Types exposing (..)
import Coders exposing (tupleDecoder)
import Json.Encode
import Json.Decode exposing (decodeValue)


sendInfoOutside : InfoForOutside -> Cmd msg
sendInfoOutside info =
  case info of
    Alert str ->
      infoForOutside
        { tag = "Alert"
        , data = Json.Encode.string str
        }

    ConfirmCancel id origContent ->
      infoForOutside
        { tag = "ConfirmCancel"
        , data = Json.Encode.list [ Json.Encode.string id, Json.Encode.string origContent ]
        }
          |> Debug.log "ConfirmCancel at sendInfoOutside"

    _ ->
      Cmd.none


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
  infoForElm
    (\outsideInfo ->
        case outsideInfo.tag of
          "Reset" ->
            tagger <| Reset

          "Load" ->
            case decodeValue ( tupleDecoder Json.Decode.string Json.Decode.value ) outsideInfo.data of
              Ok ( filepath, json ) ->
                tagger <| Load (filepath, json)

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

          "DoExportJSON" ->
            tagger <| DoExportJSON

          "DoExportTXT" ->
            tagger <| DoExportTXT

          "Keyboard" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok shortcut ->
                tagger <| Keyboard shortcut

              Err e ->
                onError e

          _ ->
            onError <| "Unexpected info from outside: " ++ toString outsideInfo
    )

port infoForOutside : OutsideData -> Cmd msg

port infoForElm : (OutsideData -> msg) -> Sub msg
