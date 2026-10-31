module Doc.ContactForm exposing (Model, Msg, init, update, view)

import Html exposing (Html, a, button, div, input, p, span, text)
import Html.Attributes exposing (attribute, class, classList, href, id, readonly, rel, target, type_, value)
import Html.Events exposing (onClick)
import SharedUI exposing (modalWrapper)
import Translation exposing (Language, TranslationId(..), tr)
import Url.Builder exposing (string, toQuery)



-- MODEL


type alias Model =
    { accountEmail : String
    , requestType : RequestType
    }


type RequestType
    = Standard
    | Urgent


init : String -> Model
init accountEmail =
    { accountEmail = accountEmail
    , requestType = Standard
    }



-- UPDATE


type Msg
    = SelectedRequestType RequestType


update : Msg -> Model -> Model
update (SelectedRequestType requestType) model =
    { model | requestType = requestType }



-- VIEW


view : Language -> { closeMsg : msg, tagger : Msg -> msg, copyEmail : Bool -> msg } -> Model -> List (Html msg)
view lang { closeMsg, tagger, copyEmail } model =
    [ div [ id "contact-options" ]
        [ div [ id "contact-tabs", attribute "role" "tablist" ]
            [ viewTab tagger model.requestType Standard
            , viewTab tagger model.requestType Urgent
            ]
        , viewPanel model
        , viewFallback copyEmail model.requestType
        ]
    ]
        |> modalWrapper closeMsg Nothing (Just [ ( "contact-modal", True ) ]) (tr lang ContactSupport)


viewTab : (Msg -> msg) -> RequestType -> RequestType -> Html msg
viewTab tagger selected requestType =
    button
        [ id ("contact-tab-" ++ idSuffix requestType)
        , type_ "button"
        , attribute "role" "tab"
        , attribute "aria-selected"
            (if selected == requestType then
                "true"

             else
                "false"
            )
        , classList
            [ ( "contact-tab", True )
            , ( "active", selected == requestType )
            , ( "urgent", requestType == Urgent )
            ]
        , onClick (tagger (SelectedRequestType requestType))
        ]
        [ text (tabTitle requestType) ]


viewPanel : Model -> Html msg
viewPanel { accountEmail, requestType } =
    div
        [ id "contact-panel"
        , attribute "role" "tabpanel"
        , classList [ ( "urgent", requestType == Urgent ) ]
        ]
        [ p [] [ text (description requestType) ]
        , div [ class "contact-panel-footer" ]
            [ a
                [ id ("contact-mailto-" ++ idSuffix requestType)
                , class "contact-mailto"
                , href (mailto (supportAddress requestType) (emailSubject requestType) accountEmail)
                , target "_blank"
                , rel "noopener"
                ]
                [ text "Email Support" ]
            ]
        ]


viewFallback : (Bool -> msg) -> RequestType -> Html msg
viewFallback copyEmail requestType =
    div [ id "contact-fallback" ]
        [ span [] [ text "Button not working? Send an email to" ]
        , div [ class "contact-address" ]
            [ input
                [ id "contact-address-field"
                , type_ "text"
                , readonly True
                , value (supportAddress requestType)
                ]
                []
            , button
                [ id ("email-copy-btn-" ++ idSuffix requestType)
                , class "email-copy-btn"
                , type_ "button"
                , onClick (copyEmail (requestType == Urgent))
                ]
                [ text "copy" ]
            ]
        ]


idSuffix : RequestType -> String
idSuffix requestType =
    case requestType of
        Standard ->
            "standard"

        Urgent ->
            "urgent"


tabTitle : RequestType -> String
tabTitle requestType =
    case requestType of
        Standard ->
            "Standard Request"

        Urgent ->
            "Urgent Request"


description : RequestType -> String
description requestType =
    case requestType of
        Standard ->
            "Checked once per week."

        Urgent ->
            "For outages or data loss only. This sends a notification to the developer's phone."


supportAddress : RequestType -> String
supportAddress requestType =
    case requestType of
        Standard ->
            "{%SUPPORT_EMAIL%}"

        Urgent ->
            "{%SUPPORT_URGENT_EMAIL%}"


emailSubject : RequestType -> String
emailSubject requestType =
    case requestType of
        Standard ->
            "Could you help me with this?"

        Urgent ->
            "Urgent: Could you help me with this?"


mailto : String -> String -> String -> String
mailto address subject accountEmail =
    ("mailto:" ++ address)
        ++ toQuery
            [ string "subject" subject
            , string "body" ("\n\n\n--\nGingko Writer account: " ++ accountEmail)
            ]
