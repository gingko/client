module Doc.ContactForm exposing (Model, Msg, init, send, update, view)

import Html exposing (Html, a, br, button, div, form, input, label, p, small, text, textarea)
import Html.Attributes exposing (href, id, placeholder, readonly, rows, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Encode as Enc
import SharedUI exposing (modalWrapper)
import Translation exposing (Language, TranslationId(..), tr)
import Url.Builder exposing (string, toQuery)



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
    , subjectField = "Could you help me with this?"
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


view : Language -> { closeMsg : msg, submitMsg : Model -> msg, tagger : Msg -> msg, copyEmail : msg } -> Model -> List (Html msg)
view lang { closeMsg, submitMsg, tagger, copyEmail } model =
    [ form [ id "contact-form", onSubmit <| submitMsg model ]
        [ input
            [ id "contact-from-email"
            , type_ "email"
            , value model.fromField
            , placeholder "Your Email"
            , onInput <| tagger << UpdateFromField
            ]
            []
        , input [ id "contact-subject", value model.subjectField, onInput <| tagger << UpdateSubjectField ] []
        , textarea [ id "contact-body", onInput <| tagger << UpdateBodyField, rows 7 ] []
        , button [ id "contact-send", type_ "submit" ] [ text "Send Now" ]
        ]
    , p [] [ text "or" ]
    , p [] [ text "send an email instead : ", br [] [], a [ href <| mailto model ] [ text "{%SUPPORT_EMAIL%}" ], small [ id "email-copy-btn", onClick copyEmail ] [ text "copy" ] ]
    ]
        |> modalWrapper closeMsg Nothing (tr lang EmailSupport)


mailto : Model -> String
mailto model =
    "mailto:{%SUPPORT_EMAIL%}"
        ++ toQuery
            [ string "subject" model.subjectField
            , string "body" model.bodyField
            ]
