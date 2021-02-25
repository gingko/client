module Doc.ContactForm exposing (Model, init, view)

import Html exposing (Html, br, button, div, form, input, label, text, textarea)
import Html.Attributes exposing (id, placeholder, readonly, type_, value)
import SharedUI exposing (modalWrapper)
import Translation exposing (Language, TranslationId(..), tr)



-- MODEL


type alias Model =
    { fromField : String
    , bodyField : String
    , subjectField : String
    }


init : String -> Model
init email =
    { fromField = email
    , bodyField = ""
    , subjectField = "Please help!"
    }



-- VIEW


view : Language -> msg -> Model -> List (Html msg)
view lang closeMsg model =
    [ form [ id "contact-form" ]
        [ label [] [ text "From:" ]
        , input [ id "contact-from-email", type_ "email", value model.fromField, placeholder "Your Email" ] []
        , br [] []
        , label [] [ text "To:" ]
        , input [ id "contact-to-email", readonly True, value "{%SUPPORT_EMAIL%}" ] []
        , br [] []
        , label [] [ text "Subject:" ]
        , input [ id "contact-subject", value model.subjectField ] []
        , br [] []
        , textarea [ id "contact-body" ] []
        , button [] [ text "Send" ]
        ]
    ]
        |> modalWrapper closeMsg Nothing (tr lang EmailSupport)
