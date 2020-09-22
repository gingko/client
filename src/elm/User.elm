port module User exposing (User, changes, db, decode, guest, loggedIn, logout, name, navKey, save, seed)

import Browser.Navigation as Nav
import Json.Decode as Json
import Outgoing exposing (Msg(..), send)
import Utils exposing (hexEncode)


type User
    = LoggedIn Nav.Key Int String
    | Guest Nav.Key



-- DATA


navKey : User -> Nav.Key
navKey user =
    case user of
        LoggedIn key _ _ ->
            key

        Guest key ->
            key


name : User -> Maybe String
name user =
    case user of
        LoggedIn _ _ email ->
            Just email

        Guest _ ->
            Nothing


seed : User -> Int
seed user =
    case user of
        LoggedIn _ s _ ->
            s

        Guest _ ->
            123456


db : User -> Maybe String
db user =
    case user of
        LoggedIn _ _ email ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ ->
            Nothing


loggedIn : User -> Bool
loggedIn user =
    case user of
        LoggedIn _ _ _ ->
            True

        Guest _ ->
            False


fromData : Nav.Key -> Int -> Maybe String -> User
fromData key initSeed maybeEmail =
    case maybeEmail of
        Just email ->
            LoggedIn key initSeed email

        Nothing ->
            Guest key


decode : Nav.Key -> Json.Value -> User
decode key json =
    let
        decoder =
            Json.map2 Tuple.pair
                (Json.field "seed" Json.int)
                (Json.field "email" Json.string)
    in
    case Json.decodeValue decoder json of
        Ok ( decodedSeed, email ) ->
            LoggedIn key decodedSeed email

        Err _ ->
            Guest key


guest : Nav.Key -> User
guest key =
    Guest key



-- Ports


save : String -> Cmd msg
save email =
    send <| StoreUser (Just email)


logout : Cmd msg
logout =
    send <| StoreUser Nothing


changes : (User -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    userStateChanged (decode key >> toMsg)


port userStateChanged : (Json.Value -> msg) -> Sub msg
