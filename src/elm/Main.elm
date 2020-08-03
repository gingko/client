module Main exposing (main)

import Browser
import Doc
import Json.Decode as Json


main : Program ( Json.Value, Doc.InitModel, Bool ) Doc.Model Doc.Msg
main =
    Browser.element
        { init = Doc.init
        , view = Doc.view
        , update = Doc.update
        , subscriptions = Doc.subscriptions
        }
