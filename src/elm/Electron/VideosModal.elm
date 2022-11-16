module Electron.VideosModal exposing (..)

import Browser
import Doc.VideoViewer as VideoViewer
import Json.Decode as Dec exposing (Decoder, Value, bool, decodeValue)
import Json.Decode.Pipeline exposing (optional, required)
import Translation exposing (Language(..), languageDecoder)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Help Videos" (VideoViewer.view m.language NoOp VideoViewerMsg m.videoViewer)
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { language : Language
    , videoViewer : VideoViewer.Model
    }


defaultModel =
    { language = En
    , videoViewer = VideoViewer.init
    }


init json =
    case decodeValue decoder json of
        Ok model ->
            ( model, Cmd.none )

        Err _ ->
            ( defaultModel, Cmd.none )


decoder : Decoder Model
decoder =
    Dec.succeed (\l -> { language = l, videoViewer = VideoViewer.init })
        |> optional "language" languageDecoder En



-- UPDATE


type Msg
    = NoOp
    | VideoViewerMsg VideoViewer.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        VideoViewerMsg videoViewerMsg ->
            ( { model | videoViewer = VideoViewer.update videoViewerMsg }, Cmd.none )
