module Translation exposing (Language(..), TranslationId(..), langFromString, languageDecoder, tr)

import Json.Decode as Json exposing (..)


type TranslationId
    = WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int
    | WordCountSubtree Int
    | WordCountGroup Int
    | WordCountColumn Int


type Language
    = En
    | Es


tr : Language -> TranslationId -> String
tr lang trans =
    let
        pluralize n str =
            if n == 1 then
                str

            else
                str ++ "s"

        translationSet =
            case trans of
                WordCountSession n ->
                    { en = "Session: " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Sesión: " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountTotal n ->
                    { en = "Total : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Total : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountCard n ->
                    { en = "Card : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Tarjeta : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountSubtree n ->
                    { en = "Subtree : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Subárbol : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountGroup n ->
                    { en = "Group : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Grupo : " ++ String.fromInt n ++ pluralize n " palabra"
                    }

                WordCountColumn n ->
                    { en = "Column : " ++ String.fromInt n ++ pluralize n " word"
                    , es = "Columna : " ++ String.fromInt n ++ pluralize n " palabra"
                    }
    in
    case lang of
        En ->
            .en translationSet

        Es ->
            .es translationSet


languageDecoder : Decoder Language
languageDecoder =
    Json.map langFromString string


langFromString : String -> Language
langFromString str =
    case str of
        "en" ->
            En

        "es" ->
            Es

        _ ->
            En
