port module User exposing (User, changes, db, fromData, guest, loggedIn, logout, name, navKey, save, seed)

import Browser.Navigation as Nav
import Json.Decode as Json
import Outgoing exposing (Msg(..), send)
import Utils exposing (hexEncode)


type User
    = LoggedIn Nav.Key Int String
    | Guest Nav.Key



-- DATA


navKey : User -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key ->
            key


name : User -> Maybe String
name session =
    case session of
        LoggedIn _ _ email ->
            Just email

        Guest _ ->
            Nothing


seed : User -> Int
seed session =
    case session of
        LoggedIn _ s _ ->
            s

        Guest _ ->
            123456


db : User -> Maybe String
db session =
    case session of
        LoggedIn _ _ email ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ ->
            Nothing


loggedIn : User -> Bool
loggedIn session =
    case session of
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


userDecoder : Nav.Key -> Json.Value -> User
userDecoder key json =
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
    userStateChanged (userDecoder key >> toMsg)


port userStateChanged : (Json.Value -> msg) -> Sub msg
