module Doc.ContactForm exposing (Model, Msg, init, send, toValue, update, view)

import Html exposing (Html, a, br, button, div, form, input, label, p, small, span, text, textarea)
import Html.Attributes as A exposing (checked, class, for, href, id, name, placeholder, readonly, rows, style, title, type_, value)
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
    , requestType : RequestType
    }


type RequestType
    = Standard
    | Urgent


init : String -> Model
init email =
    { fromField = email
    , bodyField = ""
    , subjectField = "Could you help me with this?"
    , requestType = Standard
    }



-- UPDATE


type Msg
    = UpdateFromField String
    | UpdateSubjectField String
    | UpdateBodyField String
    | UpdateRequestType RequestType


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateFromField newStr ->
            { model | fromField = newStr }

        UpdateSubjectField newStr ->
            { model | subjectField = newStr }

        UpdateBodyField newStr ->
            { model | bodyField = newStr }

        UpdateRequestType newReqType ->
            { model | requestType = newReqType }


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
        [ ( "toEmail", Enc.string <| supportEmailString model.requestType )
        , ( "fromEmail", Enc.string model.fromField )
        , ( "subject", Enc.string model.subjectField )
        , ( "body", Enc.string model.bodyField )
        ]



-- VIEW


view : Language -> { closeMsg : msg, submitMsg : Model -> msg, tagger : Msg -> msg, copyEmail : Bool -> msg } -> Model -> List (Html msg)
view lang { closeMsg, submitMsg, tagger, copyEmail } model =
    [ div [ id "contact-form" ]
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
        , div []
            [ input
                [ id "contact-urgency-standard"
                , name "contact-urgency"
                , type_ "radio"
                , value "standard"
                , onClick <| tagger <| UpdateRequestType Standard
                , checked (model.requestType == Standard)
                ]
                []
            , label [ for "contact-urgency-standard" ] [ span [] [ text "Standard Request " ], span [ id "std-req-info", class "extra-info" ] [ text " (Checked every Wednesday)" ] ]
            ]
        , div []
            [ input
                [ id "contact-urgency-urgent"
                , name "contact-urgency"
                , type_ "radio"
                , value "urgent"
                , onClick <| tagger <| UpdateRequestType Urgent
                , checked (model.requestType == Urgent)
                ]
                []
            , label [ for "contact-urgency-urgent" ] [ span [] [ text "Urgent Request " ], span [ id "urg-req-info", class "extra-info" ] [ text " (Notification to Developer's Phone)" ] ]
            ]
        , br [] []
        , button
            [ id "contact-send"
            , onClick <| submitMsg model
            , A.disabled (model.bodyField == "")
            , style "opacity"
                (if model.bodyField == "" then
                    "0.5"

                 else
                    "1"
                )
            , style "cursor"
                (if model.bodyField == "" then
                    "not-allowed"

                 else
                    "pointer"
                )
            , title
                (if model.bodyField == "" then
                    "Please enter a message"

                 else
                    ""
                )
            ]
            [ text "Send Now" ]
        ]
    , p [] [ text "or" ]
    , p []
        [ text "send an email instead : "
        , br [] []
        , a [ href <| mailto model ] [ text <| supportEmailString model.requestType ]
        , small [ id "email-copy-btn", onClick <| copyEmail (model.requestType == Urgent) ] [ text "copy" ]
        ]
    ]
        |> modalWrapper closeMsg Nothing (Just [ ( "red-alert", model.requestType == Urgent ) ]) (tr lang ContactSupport)


supportEmailString req =
    case req of
        Standard ->
            "{%SUPPORT_EMAIL%}"

        Urgent ->
            "{%SUPPORT_URGENT_EMAIL%}"


mailto : Model -> String
mailto model =
    ("mailto:" ++ supportEmailString model.requestType)
        ++ toQuery
            [ string "subject" model.subjectField
            , string "body" model.bodyField
            ]
