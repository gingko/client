port module Ports exposing (..)


import Types exposing (..)
import Coders exposing (..)


getInfoFromOutside : (InfoForElm -> msg) -> (String -> msg) -> Sub msg
getInfoFromOutside tagger onError =
  infoForElm
    (\outsideInfo ->
        case outsideInfo.tag of
          "" ->


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

