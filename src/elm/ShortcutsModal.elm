module ShortcutsModal exposing (..)

import Browser
import Doc.HelpScreen as HelpScreen
import Json.Decode as Dec exposing (Decoder, Value, bool, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Translation exposing (Language(..), languageDecoder)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = \msg mod -> ( mod, Cmd.none )
        , view = \m -> Browser.Document "Keyboard Shortcuts" (HelpScreen.viewShortcuts m.language m.isMac)
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { language : Language, isMac : Bool }


defaultModel =
    { language = En, isMac = False }


init json =
    case decodeValue decoder json of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( defaultModel, Cmd.none )


decoder : Decoder Model
decoder =
    Dec.succeed Model
        |> optional "language" languageDecoder En
        |> optional "isMac" bool False



-- UPDATE


type Msg
    = NoOp
