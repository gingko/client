module Doc.ContactForm exposing (Model, Msg, init, send, update, view)

import Html exposing (Html, br, button, div, form, input, label, text, textarea)
import Html.Attributes exposing (id, placeholder, readonly, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Encode as Enc
import SharedUI exposing (modalWrapper)
import Translation exposing (Language, TranslationId(..), tr)
import Url exposing (Protocol(..))



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



-- UPDATE


type Msg
    = UpdateFromField String
    | UpdateSubjectField String
    | UpdateBodyField String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFromField newStr ->
            { model | fromField = newStr }

        UpdateSubjectField newStr ->
            { model | subjectField = newStr }

        UpdateBodyField newStr ->
            { model | bodyField = newStr }


send : (Result Http.Error () -> msg) -> Model -> Cmd msg
send msg model =
    Http.post
        { body = Http.jsonBody (toValue model)
        , expect = Http.expectWhatever msg
        , url = "/pleasenospam"
        }


toValue : Model -> Enc.Value
toValue model =
    Enc.object
        [ ( "toEmail", Enc.string "{%SUPPORT_EMAIL%}" )
        , ( "fromEmail", Enc.string model.fromField )
        , ( "subject", Enc.string model.subjectField )
        , ( "body", Enc.string model.bodyField )
        ]



-- VIEW


view : Language -> { closeMsg : msg, submitMsg : Model -> msg, tagger : Msg -> msg } -> Model -> List (Html msg)
view lang { closeMsg, submitMsg, tagger } model =
    [ form [ id "contact-form", onSubmit <| submitMsg model ]
        [ label [] [ text "From:" ]
        , input
            [ id "contact-from-email"
            , type_ "email"
            , value model.fromField
            , placeholder "Your Email"
            , onInput <| tagger << UpdateFromField
            ]
            []
        , br [] []
        , label [] [ text "To:" ]
        , input [ id "contact-to-email", readonly True, value "{%SUPPORT_EMAIL%}" ] []
        , br [] []
        , label [] [ text "Subject:" ]
        , input [ id "contact-subject", value model.subjectField, onInput <| tagger << UpdateSubjectField ] []
        , br [] []
        , textarea [ id "contact-body", onInput <| tagger << UpdateBodyField ] []
        , button [ id "contact-send", type_ "submit" ] [ text "Send" ]
        ]
    ]
        |> modalWrapper closeMsg Nothing (tr lang EmailSupport)
