port module Ports exposing (..)


import Types exposing (..)
import Json.Decode exposing (decodeValue)


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
  infoForElm
    (\outsideInfo ->
        case outsideInfo.tag of
          "Reset" ->
            tagger <| Reset

          "Saved" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok filepath ->
                tagger <| Saved filepath

              Err e ->
                onError e

          "Keyboard" ->
            case decodeValue Json.Decode.string outsideInfo.data of
              Ok shortcut ->
                tagger <| Keyboard shortcut

              Err e ->
                onError e

          _ ->
            onError <| "Unexpected info from outside: " ++ toString outsideInfo
    )
      {-

type InfoForElm
    = Reset
    --| Load String Json.Value
    | Saved
    | DoExportJSON
    | DoExportTXT
    | Keyboard String
      -}


port infoForElm : (OutsideData -> msg) -> Sub msg
