module Utils exposing (delay, emptyText, getFieldErrors, gravatar, hash, hexEncode, myDebug, onClickStop, text, textNoTr)

{--import DebugToJson exposing (pp)--}

import Hex
import Html exposing (Html)
import Html.Events exposing (stopPropagationOn)
import Json.Decode as Dec
import Murmur3 exposing (hashString)
import Process
import SHA256
import Task
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


hash : Int -> String -> String
hash seed str =
    hashString seed str
        |> Hex.toString


delay : Int -> msg -> Cmd msg
delay ms msg =
    Task.perform (always msg) (Process.sleep <| toFloat ms)


gravatar : Int -> String -> String
gravatar size rawEmail =
    let
        email =
            rawEmail |> String.trim |> String.toLower

        emailHash =
            email
                |> SHA256.fromString
                |> SHA256.toHex
    in
    "https://www.gravatar.com/avatar/" ++ emailHash ++ "?s=" ++ String.fromInt size ++ "&d=identicon"



-- Debugging


myDebug : String -> a -> a
myDebug label value =
    let
        _ =
            --Debug.log label (pp (Debug.toString value))
            ()
    in
    value



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
