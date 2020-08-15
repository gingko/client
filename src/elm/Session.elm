port module Session exposing (Session, changes, fromData, loggedIn, logout, navKey, save, userDb, username)

import Browser.Navigation as Nav
import Json.Decode as Json
import Utils exposing (hexEncode)


type Session
    = LoggedIn Nav.Key String
    | Guest Nav.Key



-- DATA


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key


username : Session -> Maybe String
username session =
    case session of
        LoggedIn _ email ->
            Just email

        Guest _ ->
            Nothing


userDb : Session -> Maybe String
userDb session =
    case session of
        LoggedIn _ email ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ ->
            Nothing


loggedIn : Session -> Bool
loggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        Guest _ ->
            False


fromData : Nav.Key -> Maybe String -> Session
fromData key maybeEmail =
    case maybeEmail of
        Just email ->
            LoggedIn key email

        Nothing ->
            Guest key


sessionDecoder : Nav.Key -> Json.Value -> Session
sessionDecoder key json =
    case Json.decodeValue Json.string json of
        Ok email ->
            LoggedIn key email

        Err _ ->
            Guest key



-- Ports


save : String -> Cmd msg
save email =
    storeSession (Just email)


logout : Cmd msg
logout =
    storeSession Nothing


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    sessionChanged (sessionDecoder key >> toMsg)


port storeSession : Maybe String -> Cmd msg


port sessionChanged : (Json.Value -> msg) -> Sub msg
