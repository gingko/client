module Doc.UIStyled exposing (viewAINewPrompt)

import Css exposing (..)
import Html exposing (Html)
import Html.Styled exposing (div, label, text, textarea, toUnstyled)
import Html.Styled.Attributes exposing (autofocus, css, for, id, rows)
import Html.Styled.Events exposing (onInput)
import SharedUI exposing (modalWrapper)
import Styles exposing (colors)
import Translation exposing (Language)


viewAINewPrompt : Language -> { modalClosed : msg, promptInput : String -> msg } -> List (Html msg)
viewAINewPrompt lang msgs =
    [ div [ css [ displayFlex, flexDirection column, property "gap" "10px" ] ]
        [ label [ for "ai-new-prompt", css [ fontWeight bold ] ] [ text "Prompt" ]
        , textarea
            [ id "ai-new-prompt"
            , css [ fontSize (px 16), padding (px 10) ]
            , onInput msgs.promptInput
            , autofocus True
            , rows 8
            ]
            []
        , div [ css [ displayFlex, justifyContent flexEnd ] ]
            [ div
                [ css
                    [ fontWeight bold
                    , color colors.nearWhite
                    , backgroundColor colors.green
                    , padding (px 10)
                    , borderRadius (px 6)
                    , cursor pointer
                    , hover [ boxShadow4 zero (px 2) (px 3) (rgba 0 0 0 0.2) ]
                    , active [ marginTop (px 1) ]
                    ]
                ]
                [ text "Generate" ]
            ]
        ]
    ]
        |> List.map toUnstyled
        |> modalWrapper msgs.modalClosed Nothing Nothing "Generate Document"
