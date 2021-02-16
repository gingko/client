module Import.Template exposing (Template(..), fetchJSON, fromString, toString)

import Http
import Json.Decode as Json exposing (Decoder)


type Template
    = WelcomeTree
    | Timeline
    | AcademicPaper
    | HerosJourney


fromString : String -> Maybe Template
fromString str =
    case str of
        "welcome" ->
            Just WelcomeTree

        "timeline" ->
            Just Timeline

        "academic-paper" ->
            Just AcademicPaper

        "heros-journey" ->
            Just HerosJourney

        _ ->
            Nothing


toString : Template -> String
toString template =
    case template of
        WelcomeTree ->
            "welcome"

        Timeline ->
            "timeline"

        AcademicPaper ->
            "academic-paper"

        HerosJourney ->
            "heros-journey"


srcUrl : Template -> String
srcUrl template =
    "/templates/" ++ toString template ++ ".json"


fetchJSON : (Result Http.Error a -> msg) -> Decoder a -> Template -> Cmd msg
fetchJSON toMsg jsonDecoder template =
    Http.get { expect = Http.expectJson toMsg jsonDecoder, url = srcUrl template }
