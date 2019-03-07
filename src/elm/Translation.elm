module Translation exposing (Language(..), TranslationId(..), tr)


type TranslationId
    = WordCountSession Int
    | WordCountTotal Int
    | WordCountCard Int


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
    in
    case lang of
        En ->
            .en translationSet

        Es ->
            .es translationSet
