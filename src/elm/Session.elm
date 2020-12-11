port module Session exposing (Session, db, decode, fileMenuOpen, language, lastDocId, loggedIn, loginChanges, logout, name, navKey, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, seed, setFileOpen, setLanguage, setSeed, setShortcutTrayOpen, settingsChange, shortcutTrayOpen, storeLogin, storeSignup)

import Browser.Navigation as Nav
import Doc.List as DocList
import Http
import Json.Decode as Dec
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Random
import Translation exposing (Language(..), langFromString, langToString, languageDecoder)
import Utils exposing (hexEncode)



-- MODEL


type Session
    = LoggedIn SessionData UserData
    | Guest SessionData GuestData


type alias SessionData =
    -- Not persisted
    { navKey : Nav.Key
    , seed : Random.Seed
    , fileMenuOpen : Bool
    , lastDocId : Maybe String
    }


type alias UserData =
    -- Persisted in userdb settings
    { email : String
    , language : Translation.Language
    , shortcutTrayOpen : Bool
    , documents : DocList.Model
    }


type alias GuestData =
    -- Persisted in localStorage
    { language : Translation.Language
    }



-- GETTERS


getFromSession : (SessionData -> a) -> Session -> a
getFromSession getter user =
    case user of
        LoggedIn session _ ->
            getter session

        Guest session _ ->
            getter session


navKey : Session -> Nav.Key
navKey user =
    getFromSession .navKey user


name : Session -> Maybe String
name user =
    case user of
        LoggedIn _ { email } ->
            Just email

        Guest _ _ ->
            Nothing


seed : Session -> Random.Seed
seed user =
    getFromSession .seed user


lastDocId : Session -> Maybe String
lastDocId user =
    getFromSession .lastDocId user


db : Session -> Maybe String
db user =
    case user of
        LoggedIn _ { email } ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ _ ->
            Nothing


fileMenuOpen : Session -> Bool
fileMenuOpen user =
    getFromSession .fileMenuOpen user


language : Session -> Language
language user =
    case user of
        LoggedIn _ data ->
            data.language

        Guest _ data ->
            data.language


shortcutTrayOpen : Session -> Bool
shortcutTrayOpen user =
    case user of
        LoggedIn _ data ->
            data.shortcutTrayOpen

        Guest _ _ ->
            False


loggedIn : Session -> Bool
loggedIn user =
    case user of
        LoggedIn _ _ ->
            True

        Guest _ _ ->
            False



-- UPDATE


updateSession : (SessionData -> SessionData) -> Session -> Session
updateSession updateFn user =
    case user of
        LoggedIn session data ->
            LoggedIn (updateFn session) data

        Guest session data ->
            Guest (updateFn session) data


setSeed : Random.Seed -> Session -> Session
setSeed newSeed user =
    updateSession (\s -> { s | seed = newSeed }) user


setFileOpen : Bool -> Session -> Session
setFileOpen isOpen user =
    updateSession (\s -> { s | fileMenuOpen = isOpen }) user


setLanguage : Language -> Session -> Session
setLanguage lang user =
    case user of
        LoggedIn key data ->
            LoggedIn key { data | language = lang }

        Guest key data ->
            Guest key { data | language = lang }


setShortcutTrayOpen : Bool -> Session -> Session
setShortcutTrayOpen isOpen user =
    case user of
        LoggedIn key data ->
            LoggedIn key { data | shortcutTrayOpen = isOpen }

        Guest _ _ ->
            user



-- ENCODER & DECODER


decode : Nav.Key -> Dec.Value -> Session
decode key json =
    case Dec.decodeValue (decoder key) json of
        Ok user ->
            user

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
            Guest (SessionData key errToSeed False Nothing) (GuestData En)


decoder : Nav.Key -> Dec.Decoder Session
decoder key =
    Dec.oneOf [ decodeLoggedIn key, decodeGuest key ]


decodeLoggedIn : Nav.Key -> Dec.Decoder Session
decodeLoggedIn key =
    Dec.succeed
        (\email s lang trayOpen lastDoc ->
            LoggedIn (SessionData key s False lastDoc) (UserData email lang trayOpen DocList.init)
        )
        |> required "email" Dec.string
        |> required "seed" (Dec.int |> Dec.map Random.initialSeed)
        |> optional "language" (Dec.string |> Dec.map langFromString) En
        |> optional "shortcutTrayOpen" Dec.bool True
        |> optional "lastDocId" (Dec.maybe Dec.string) Nothing


decodeGuest : Nav.Key -> Dec.Decoder Session
decodeGuest key =
    Dec.map2 (\s l -> Guest (SessionData key s False Nothing) (GuestData l))
        (Dec.field "seed" (Dec.int |> Dec.map Random.initialSeed))
        (Dec.field "language" (Dec.string |> Dec.map langFromString))


responseDecoder : Session -> Dec.Decoder Session
responseDecoder user =
    let
        builder email lang trayOpen =
            case user of
                Guest session data ->
                    LoggedIn session (UserData email lang trayOpen DocList.init)

                LoggedIn _ _ ->
                    user
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optionalAt [ "settings", "language" ] (Dec.map langFromString Dec.string) En
        |> optionalAt [ "settings", "shortcutTrayOpen" ] Dec.bool True


encode : Session -> Enc.Value
encode user =
    case user of
        LoggedIn _ data ->
            Enc.object
                [ ( "email", Enc.string data.email )
                , ( "language", data.language |> langToString |> Enc.string )
                ]

        Guest _ data ->
            Enc.object
                [ ( "language", data.language |> langToString |> Enc.string )
                ]



-- AUTHENTICATION


requestSignup : (Result Http.Error Session -> msg) -> String -> String -> Session -> Cmd msg
requestSignup toMsg email password user =
    let
        requestBody =
            Enc.object
                [ ( "email", Enc.string email )
                , ( "password", Enc.string password )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/signup"
        , body = requestBody
        , expect = Http.expectJson toMsg (responseDecoder user)
        }


storeSignup : Session -> Cmd msg
storeSignup user =
    store (Just user)


requestLogin : (Result Http.Error Session -> msg) -> String -> String -> Session -> Cmd msg
requestLogin toMsg email password user =
    let
        requestBody =
            Enc.object
                [ ( "email", Enc.string email )
                , ( "password", Enc.string password )
                ]
                |> Http.jsonBody
    in
    Http.riskyRequest
        { method = "POST"
        , url = "/login"
        , headers = []
        , body = requestBody
        , expect = Http.expectJson toMsg (responseDecoder user)
        , timeout = Nothing
        , tracker = Nothing
        }


storeLogin : Session -> Cmd msg
storeLogin user =
    store (Just user)


requestForgotPassword : (Result Http.Error Session -> msg) -> String -> Session -> Cmd msg
requestForgotPassword toMsg email user =
    let
        requestBody =
            Enc.object
                [ ( "email", Enc.string email )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/forgot-password"
        , body = requestBody
        , expect = Http.expectJson toMsg (responseDecoder user)
        }


requestResetPassword : (Result Http.Error Session -> msg) -> { newPassword : String, token : String } -> Session -> Cmd msg
requestResetPassword toMsg { newPassword, token } user =
    let
        requestBody =
            Enc.object
                [ ( "token", Enc.string token )
                , ( "password", Enc.string newPassword )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/reset-password"
        , body = requestBody
        , expect = Http.expectJson toMsg (responseDecoder user)
        }


logout : Cmd msg
logout =
    store Nothing



-- PORTS


store : Maybe Session -> Cmd msg
store user_ =
    case user_ of
        Just user ->
            send <| StoreUser (encode user)

        Nothing ->
            send <| StoreUser Enc.null


loginChanges : (Session -> msg) -> Nav.Key -> Sub msg
loginChanges toMsg key =
    userLoginChange (decode key >> toMsg)


settingsChange : (Language -> msg) -> Sub msg
settingsChange toMsg =
    let
        decodeSettings json =
            case Dec.decodeValue (Dec.field "language" languageDecoder) json of
                Ok lang ->
                    lang

                Err err ->
                    En
    in
    userSettingsChange (decodeSettings >> toMsg)


port userLoginChange : (Dec.Value -> msg) -> Sub msg


port userSettingsChange : (Dec.Value -> msg) -> Sub msg
