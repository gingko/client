port module Session exposing (PaymentStatus(..), Session, currentTime, db, decode, documents, fileMenuOpen, fromLegacy, isMac, language, lastDocId, loggedIn, loginChanges, logout, name, navKey, paymentStatus, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, seed, setFileOpen, setLanguage, setSeed, setShortcutTrayOpen, shortcutTrayOpen, storeLogin, storeSignup, sync, updateDocuments, updateTime, updateUpgrade, upgradeModel, userSettingsChange)

import Browser.Navigation as Nav
import Doc.List as DocList
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Random
import Time
import Translation exposing (Language(..), langFromString, langToString, languageDecoder)
import Upgrade
import Utils exposing (hexEncode)



-- MODEL


type Session
    = LoggedIn SessionData UserData
    | Guest SessionData GuestData


type alias SessionData =
    -- Not persisted
    { navKey : Nav.Key
    , seed : Random.Seed
    , isMac : Bool
    , currentTime : Time.Posix
    , fileMenuOpen : Bool
    , lastDocId : Maybe String
    , fromLegacy : Bool
    }


type alias UserData =
    { email : String
    , language : Translation.Language
    , upgradeModel : Upgrade.Model
    , paymentStatus : PaymentStatus
    , shortcutTrayOpen : Bool
    , documents : DocList.Model
    }


type PaymentStatus
    = Unknown
    | Trial Time.Posix
    | Customer String


type alias GuestData =
    -- Persisted in localStorage
    { language : Translation.Language
    }



-- GETTERS


getFromSession : (SessionData -> a) -> Session -> a
getFromSession getter session =
    case session of
        LoggedIn sessionData _ ->
            getter sessionData

        Guest sessionData _ ->
            getter sessionData


navKey : Session -> Nav.Key
navKey session =
    getFromSession .navKey session


name : Session -> Maybe String
name session =
    case session of
        LoggedIn _ { email } ->
            Just email

        Guest _ _ ->
            Nothing


seed : Session -> Random.Seed
seed session =
    getFromSession .seed session


isMac : Session -> Bool
isMac session =
    getFromSession .isMac session


currentTime : Session -> Time.Posix
currentTime session =
    getFromSession .currentTime session


lastDocId : Session -> Maybe String
lastDocId session =
    getFromSession .lastDocId session


fromLegacy : Session -> Bool
fromLegacy session =
    getFromSession .fromLegacy session


db : Session -> Maybe String
db session =
    case session of
        LoggedIn _ { email } ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ _ ->
            Nothing


fileMenuOpen : Session -> Bool
fileMenuOpen session =
    getFromSession .fileMenuOpen session


language : Session -> Language
language session =
    case session of
        LoggedIn _ data ->
            data.language

        Guest _ data ->
            data.language


upgradeModel : Session -> Maybe Upgrade.Model
upgradeModel session =
    case session of
        LoggedIn _ data ->
            Just data.upgradeModel

        Guest _ _ ->
            Nothing


paymentStatus : Session -> PaymentStatus
paymentStatus session =
    case session of
        LoggedIn _ data ->
            data.paymentStatus

        Guest _ _ ->
            Unknown


shortcutTrayOpen : Session -> Bool
shortcutTrayOpen session =
    case session of
        LoggedIn _ data ->
            data.shortcutTrayOpen

        Guest _ _ ->
            False


documents : Session -> DocList.Model
documents session =
    case session of
        LoggedIn _ data ->
            data.documents

        Guest _ _ ->
            DocList.init


loggedIn : Session -> Bool
loggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        Guest _ _ ->
            False



-- UPDATE


sync : Dec.Value -> Session -> Session
sync json session =
    let
        settingsDecoder : Decoder { language : Language, paymentStatus : PaymentStatus }
        settingsDecoder =
            Dec.succeed (\lang payStat -> { language = lang, paymentStatus = payStat })
                |> optional "language" languageDecoder En
                |> optional "paymentStatus" decodePaymentStatus Unknown
    in
    case ( Dec.decodeValue settingsDecoder json, session ) of
        ( Ok newSettings, LoggedIn sessData userData ) ->
            LoggedIn sessData { userData | language = newSettings.language, paymentStatus = newSettings.paymentStatus }

        ( Ok newSettings, Guest sessData guestData ) ->
            Guest sessData { guestData | language = newSettings.language }

        ( Err _, _ ) ->
            session


updateSession : (SessionData -> SessionData) -> Session -> Session
updateSession updateFn session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn (updateFn sessionData) data

        Guest sessionData data ->
            Guest (updateFn sessionData) data


setSeed : Random.Seed -> Session -> Session
setSeed newSeed session =
    updateSession (\s -> { s | seed = newSeed }) session


updateTime : Time.Posix -> Session -> Session
updateTime newTime session =
    updateSession (\s -> { s | currentTime = newTime }) session


setFileOpen : Bool -> Session -> Session
setFileOpen isOpen session =
    updateSession (\s -> { s | fileMenuOpen = isOpen }) session


setLanguage : Language -> Session -> Session
setLanguage lang session =
    case session of
        LoggedIn key data ->
            LoggedIn key { data | language = lang }

        Guest key data ->
            Guest key { data | language = lang }


setShortcutTrayOpen : Bool -> Session -> Session
setShortcutTrayOpen isOpen session =
    case session of
        LoggedIn key data ->
            LoggedIn key { data | shortcutTrayOpen = isOpen }

        Guest _ _ ->
            session


updateDocuments : DocList.Model -> Session -> Session
updateDocuments docList session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn sessionData { data | documents = DocList.update docList data.documents }

        Guest _ _ ->
            session


updateUpgrade : Upgrade.Msg -> Session -> Session
updateUpgrade upgradeMsg session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn sessionData { data | upgradeModel = Upgrade.update upgradeMsg data.upgradeModel }

        Guest _ _ ->
            session



-- ENCODER & DECODER


decode : Nav.Key -> Dec.Value -> Session
decode key json =
    case Dec.decodeValue (decoder key) json of
        Ok session ->
            session

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
            Guest
                { navKey = key
                , seed = errToSeed
                , isMac = False
                , currentTime = Time.millisToPosix 0
                , fileMenuOpen = False
                , lastDocId = Nothing
                , fromLegacy = False
                }
                (GuestData En)


decoder : Nav.Key -> Dec.Decoder Session
decoder key =
    Dec.oneOf [ decodeLoggedIn key, decodeGuest key ]


decodeLoggedIn : Nav.Key -> Dec.Decoder Session
decodeLoggedIn key =
    Dec.succeed
        (\email s os t legacy lang side payStat trayOpen lastDoc ->
            LoggedIn
                { navKey = key
                , seed = s
                , isMac = os
                , currentTime = t
                , fileMenuOpen = side
                , lastDocId = Nothing
                , fromLegacy = legacy
                }
                (UserData email lang Upgrade.init payStat trayOpen DocList.init)
        )
        |> required "email" Dec.string
        |> required "seed" (Dec.int |> Dec.map Random.initialSeed)
        |> required "isMac" Dec.bool
        |> required "currentTime" (Dec.int |> Dec.map Time.millisToPosix)
        |> optional "fromLegacy" Dec.bool False
        |> optional "language" (Dec.string |> Dec.map langFromString) En
        |> optional "sidebarOpen" Dec.bool False
        |> optional "paymentStatus" decodePaymentStatus Unknown
        |> optional "shortcutTrayOpen" Dec.bool False
        |> optional "lastDocId" (Dec.maybe Dec.string) Nothing


decodePaymentStatus : Dec.Decoder PaymentStatus
decodePaymentStatus =
    Dec.oneOf
        [ Dec.succeed Customer |> required "customer" Dec.string
        , Dec.succeed Trial |> required "trialExpires" (Dec.int |> Dec.map Time.millisToPosix)
        ]


decodeGuest : Nav.Key -> Dec.Decoder Session
decodeGuest key =
    Dec.succeed
        (\s os t legacy l side ->
            Guest
                { navKey = key
                , seed = s
                , isMac = os
                , currentTime = t
                , fileMenuOpen = side
                , lastDocId = Nothing
                , fromLegacy = legacy
                }
                (GuestData l)
        )
        |> required "seed" (Dec.int |> Dec.map Random.initialSeed)
        |> required "isMac" Dec.bool
        |> required "currentTime" (Dec.int |> Dec.map Time.millisToPosix)
        |> optional "fromLegacy" Dec.bool False
        |> optional "language" (Dec.string |> Dec.map langFromString) En
        |> optional "sidebarOpen" Dec.bool False


responseDecoder : Session -> Dec.Decoder Session
responseDecoder session =
    let
        builder email lang payStat trayOpen =
            case session of
                Guest sessionData data ->
                    LoggedIn sessionData (UserData email lang Upgrade.init payStat trayOpen DocList.init)

                LoggedIn _ _ ->
                    session
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optionalAt [ "settings", "language" ] (Dec.map langFromString Dec.string) En
        |> optionalAt [ "settings", "paymentStatus" ] decodePaymentStatus Unknown
        |> optionalAt [ "settings", "shortcutTrayOpen" ] Dec.bool False


encode : Session -> Enc.Value
encode session =
    case session of
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
requestSignup toMsg email password session =
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
        , expect = Http.expectJson toMsg (responseDecoder session)
        }


storeSignup : Session -> Cmd msg
storeSignup session =
    store (Just session)


requestLogin : (Result Http.Error Session -> msg) -> String -> String -> Session -> Cmd msg
requestLogin toMsg email password session =
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
        , expect = Http.expectJson toMsg (responseDecoder session)
        , timeout = Nothing
        , tracker = Nothing
        }


storeLogin : Session -> Cmd msg
storeLogin session =
    store (Just session)


requestForgotPassword : (Result Http.Error Session -> msg) -> String -> Session -> Cmd msg
requestForgotPassword toMsg email session =
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
        , expect = Http.expectJson toMsg (responseDecoder session)
        }


requestResetPassword : (Result Http.Error Session -> msg) -> { newPassword : String, token : String } -> Session -> Cmd msg
requestResetPassword toMsg { newPassword, token } session =
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
        , expect = Http.expectJson toMsg (responseDecoder session)
        }


logout : Cmd msg
logout =
    store Nothing



-- PORTS


store : Maybe Session -> Cmd msg
store session_ =
    case session_ of
        Just session ->
            send <| StoreUser (encode session)

        Nothing ->
            send <| StoreUser Enc.null


loginChanges : (Session -> msg) -> Nav.Key -> Sub msg
loginChanges toMsg key =
    userLoginChange (decode key >> toMsg)


port userLoginChange : (Dec.Value -> msg) -> Sub msg


port userSettingsChange : (Dec.Value -> msg) -> Sub msg
