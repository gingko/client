port module Session exposing (Session, changes, fromData, guest, loggedIn, logout, navKey, save, seed, userDb, username)

import Browser.Navigation as Nav
import Json.Decode as Json
import Ports exposing (OutgoingMsg(..), sendOut)
import Utils exposing (hexEncode)


type Session
    = LoggedIn Nav.Key Int String
    | Guest Nav.Key



-- DATA


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ _ ->
            key

        Guest key ->
            key


username : Session -> Maybe String
username session =
    case session of
        LoggedIn _ _ email ->
            Just email

        Guest _ ->
            Nothing


seed : Session -> Int
seed session =
    case session of
        LoggedIn _ s _ ->
            s

        Guest _ ->
            123456


userDb : Session -> Maybe String
userDb session =
    case session of
        LoggedIn _ _ email ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ ->
            Nothing


loggedIn : Session -> Bool
loggedIn session =
    case session of
        LoggedIn _ _ _ ->
            True

        Guest _ ->
            False


fromData : Nav.Key -> Int -> Maybe String -> Session
fromData key initSeed maybeEmail =
    case maybeEmail of
        Just email ->
            LoggedIn key initSeed email

        Nothing ->
            Guest key


sessionDecoder : Nav.Key -> Json.Value -> Session
sessionDecoder key json =
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


guest : Nav.Key -> Session
guest key =
    Guest key



-- Ports


save : String -> Cmd msg
save email =
    sendOut <| StoreSession (Just email)


logout : Cmd msg
logout =
    sendOut <| StoreSession Nothing


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    sessionChanged (sessionDecoder key >> toMsg)


port sessionChanged : (Json.Value -> msg) -> Sub msg
