module Utils exposing (decodeDebug, emptyText, getFieldErrors, hexEncode, onClickStop, randomPositiveInt, text, textNoTr)

import Hex
import Html exposing (Html)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Dec exposing (Decoder)
import Random
import Translation exposing (Language, TranslationId, tr)


onClickStop : msg -> Html.Attribute msg
onClickStop msg =
    stopPropagationOn "click" (Dec.succeed ( msg, True ))


hexEncode : String -> String
hexEncode input =
    input
        |> String.toList
        |> List.map Char.toCode
        |> List.map Hex.toString
        |> String.join ""


randomPositiveInt : Random.Generator Int
randomPositiveInt =
    Random.int 0 Random.maxInt



-- Translation Helper Function


text : Language -> TranslationId -> Html msg
text lang tid =
    Html.text <| tr lang tid


textNoTr : String -> Html msg
textNoTr str =
    Html.text str


emptyText : Html msg
emptyText =
    Html.text ""


getFieldErrors : field -> List ( field, a ) -> List a
getFieldErrors field errs =
    errs
        |> List.filter ((==) field << Tuple.first)
        |> List.map Tuple.second


decodeDebug : String -> Decoder a -> Decoder a
decodeDebug message decoder =
    Dec.value
        |> Dec.andThen (debugHelper message decoder)


debugHelper : String -> Decoder a -> Dec.Value -> Decoder a
debugHelper message decoder value =
    {-
       let
           _ =
               Debug.log message (Dec.decodeValue decoder value)
       in
    -}
    decoder
