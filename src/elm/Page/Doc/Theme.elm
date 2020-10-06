module Page.Doc.Theme exposing (Theme(..), applyTheme, decoder, toValue)

import Html
import Html.Attributes exposing (class)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type Theme
    = Default
    | Gray
    | Turquoise
    | Dark


applyTheme : Theme -> Html.Attribute msg
applyTheme theme =
    case theme of
        Default ->
            class ""

        Gray ->
            class "gray-theme"

        Turquoise ->
            class "turquoise-theme"

        Dark ->
            class "dark-theme"


toValue : Theme -> Enc.Value
toValue theme =
    case theme of
        Default ->
            Enc.string "default"

        Gray ->
            Enc.string "gray"

        Turquoise ->
            Enc.string "turquoise"

        Dark ->
            Enc.string "dark"


decoder : Decoder Theme
decoder =
    Dec.field "theme" Dec.string
        |> Dec.map
            (\s ->
                case s of
                    "default" ->
                        Default

                    "gray" ->
                        Gray

                    "turquoise" ->
                        Turquoise

                    "dark" ->
                        Dark

                    _ ->
                        Default
            )
