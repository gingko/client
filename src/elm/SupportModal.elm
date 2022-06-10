module SupportModal exposing (..)

import Browser
import Doc.ContactForm as ContactForm
import Json.Decode as Dec exposing (Decoder, Value, bool, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Translation exposing (Language(..), languageDecoder)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view =
            \m ->
                Browser.Document "Contact Support"
                    (ContactForm.view m.language
                        { closeMsg = NoOp
                        , copyEmail = CopyEmail
                        , submitMsg = SubmitMsg
                        , tagger = ContactFormMsg
                        }
                        m.contactForm
                    )
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { language : Language
    , contactForm : ContactForm.Model
    }


defaultModel =
    { language = En
    , contactForm = ContactForm.init ""
    }


init json =
    case decodeValue decoder json of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( defaultModel, Cmd.none )


decoder : Decoder Model
decoder =
    Dec.succeed (\l -> { language = l, contactForm = ContactForm.init "" })
        |> optional "language" languageDecoder En



-- UPDATE


type Msg
    = NoOp
    | CopyEmail Bool
    | SubmitMsg ContactForm.Model
    | ContactFormMsg ContactForm.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CopyEmail _ ->
            ( model, Cmd.none )

        SubmitMsg _ ->
            ( model, Cmd.none )

        ContactFormMsg contactFormMsg ->
            ( { model | contactForm = ContactForm.update contactFormMsg model.contactForm }, Cmd.none )
