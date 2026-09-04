module Page.Doc.Theme exposing (Theme(..), applyTheme, decoder, toValue)

import Html
import Html.Attributes exposing (class)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type Theme
    = Default
    | Classic
    | Gray
    | Green
    | Turquoise
    | Dark


applyTheme : Theme -> Html.Attribute msg
applyTheme theme =
    case theme of
        Default ->
            class ""

        Classic ->
            class "classic-theme"

        Gray ->
            class "gray-theme"

        Green ->
            class "green-theme"

        Turquoise ->
            class "turquoise-theme"

        Dark ->
            class "dark-theme"


toValue : Theme -> Enc.Value
toValue theme =
    case theme of
        Default ->
            Enc.string "default"

        Classic ->
            Enc.string "classic"

        Gray ->
            Enc.string "gray"

        Green ->
            Enc.string "green"

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

                    "classic" ->
                        Classic

                    "gray" ->
                        Gray

                    "green" ->
                        Green

                    "turquoise" ->
                        Turquoise

                    "dark" ->
                        Dark

                    _ ->
                        Default
            )
