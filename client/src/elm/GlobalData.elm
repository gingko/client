module GlobalData exposing (GlobalData, currentTime, decode, isMac, language, public, seed, setLanguage, setSeed, updateTime)

import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Random
import Time
import Translation exposing (Language(..), langFromString)


type GlobalData
    = GlobalData
        { seed : Random.Seed
        , currentTime : Time.Posix
        , language : Language
        , isMac : Bool
        }


decode : Dec.Value -> GlobalData
decode json =
    case Dec.decodeValue decoder json of
        Ok gData ->
            gData

        Err err ->
            let
                errToSeed =
                    err
                        |> Dec.errorToString
                        |> String.right 10
                        |> String.toList
                        |> List.map Char.toCode
                        |> List.foldl (+) 12345
                        |> Random.initialSeed
            in
            GlobalData
                { seed = errToSeed
                , currentTime = Time.millisToPosix 0
                , language = En
                , isMac = False
                }


decoder : Decoder GlobalData
decoder =
    Dec.succeed
        (\s t lang os ->
            GlobalData
                { seed = s
                , currentTime = t
                , language = lang
                , isMac = os
                }
        )
        |> required "seed" (Dec.int |> Dec.map Random.initialSeed)
        |> required "currentTime" (Dec.int |> Dec.map Time.millisToPosix)
        |> optional "language" (Dec.string |> Dec.map langFromString) En
        |> required "isMac" Dec.bool


public : GlobalData
public =
    GlobalData
        { seed = Random.initialSeed 12345
        , currentTime = Time.millisToPosix 0
        , language = En
        , isMac = False
        }



-- GETTERS


seed : GlobalData -> Random.Seed
seed (GlobalData record) =
    record.seed


currentTime : GlobalData -> Time.Posix
currentTime (GlobalData record) =
    record.currentTime


language : GlobalData -> Language
language (GlobalData record) =
    record.language


isMac : GlobalData -> Bool
isMac (GlobalData record) =
    record.isMac



-- UPDATE


setSeed : Random.Seed -> GlobalData -> GlobalData
setSeed newSeed (GlobalData record) =
    GlobalData { record | seed = newSeed }


updateTime : Time.Posix -> GlobalData -> GlobalData
updateTime newTime (GlobalData record) =
    GlobalData { record | currentTime = newTime }


setLanguage : Language -> GlobalData -> GlobalData
setLanguage newLang (GlobalData record) =
    GlobalData { record | language = newLang }
