module Session exposing (Session, changes, fromData, logout, navKey, save, username)

import Browser.Navigation as Nav
import Json.Decode as Json
import Ports


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
    Ports.storeSession (Just email)


logout : Cmd msg
logout =
    Ports.storeSession Nothing


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Ports.sessionChanged (sessionDecoder key >> toMsg)
