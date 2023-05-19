module UpdatedAt exposing (UpdatedAt, areEqual, decoder, encode, fromParts, getHash, getTimestamp, isLTE, maximum, sort, unique, uniqueBy, zero)

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import List.Extra as ListExtra



-- TYPES & CONSTRUCTORS


type UpdatedAt
    = UpdatedAt { timestamp : Int, counter : Int, hash : String }


type alias Data =
    { timestamp : Int, counter : Int, hash : String }


decoder : Decoder UpdatedAt
decoder =
    Dec.string
        |> Dec.andThen
            (\str ->
                case stringParser str of
                    Just data ->
                        Dec.succeed data

                    Nothing ->
                        Dec.fail "Invalid UpdatedAt string"
            )


fromParts : Int -> Int -> String -> UpdatedAt
fromParts timestamp counter hash =
    UpdatedAt { timestamp = timestamp, counter = counter, hash = hash }


zero : UpdatedAt
zero =
    UpdatedAt { timestamp = 0, counter = 0, hash = "" }



-- EXPOSED FUNCTIONS


sort : (a -> UpdatedAt) -> List a -> List a
sort f l =
    l
        |> List.map (\a -> ( f a, a ))
        |> List.sortWith (\( a, _ ) ( b, _ ) -> compareUpdatedAt a b)
        |> List.map Tuple.second


getTimestamp : UpdatedAt -> Int
getTimestamp (UpdatedAt data) =
    data.timestamp


getHash : UpdatedAt -> String
getHash (UpdatedAt data) =
    data.hash


areEqual : UpdatedAt -> UpdatedAt -> Bool
areEqual (UpdatedAt a) (UpdatedAt b) =
    a.timestamp == b.timestamp && a.counter == b.counter && a.hash == b.hash


unique : List UpdatedAt -> List UpdatedAt
unique l =
    l
        |> ListExtra.uniqueBy toString


uniqueBy : (a -> UpdatedAt) -> List a -> List a
uniqueBy f l =
    l
        |> ListExtra.uniqueBy (toString << f)


maximum : List UpdatedAt -> Maybe UpdatedAt
maximum l =
    l
        |> sort identity
        |> List.reverse
        |> List.head


isLTE : UpdatedAt -> UpdatedAt -> Bool
isLTE ua1 ua2 =
    compareUpdatedAt ua1 ua2
        == LT
        || areEqual ua1 ua2



-- PRIVATE FUNCTIONS


compareUpdatedAt : UpdatedAt -> UpdatedAt -> Order
compareUpdatedAt (UpdatedAt a) (UpdatedAt b) =
    case compare a.timestamp b.timestamp of
        LT ->
            LT

        GT ->
            GT

        EQ ->
            case compare a.counter b.counter of
                LT ->
                    LT

                GT ->
                    GT

                EQ ->
                    compare a.hash b.hash



-- ENCODER / DECODER


encode : UpdatedAt -> Enc.Value
encode ua =
    Enc.string (toString ua)


toString : UpdatedAt -> String
toString (UpdatedAt data) =
    case ( data.timestamp, data.counter, data.hash ) of
        ( 0, 0, "" ) ->
            "0"

        _ ->
            String.join ":" [ String.fromInt data.timestamp, String.fromInt data.counter, data.hash ]


stringParser : String -> Maybe UpdatedAt
stringParser str =
    case String.split ":" str of
        [ timestamp, counter, hash ] ->
            case ( String.toInt timestamp, String.toInt counter ) of
                ( Just ts, Just ctr ) ->
                    UpdatedAt { timestamp = ts, counter = ctr, hash = hash }
                        |> Just

                _ ->
                    Nothing

        _ ->
            Nothing
