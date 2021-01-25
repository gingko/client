port module Session exposing (Session, customer, db, decode, documents, fileMenuOpen, language, lastDocId, loggedIn, loginChanges, logout, name, navKey, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, seed, setFileOpen, setLanguage, setSeed, setShortcutTrayOpen, shortcutTrayOpen, storeLogin, storeSignup, sync, updateDocuments, updateUpgrade, upgradeModel, userSettingsChange)

import Browser.Navigation as Nav
import Doc.List as DocList
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, optionalAt, required)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Random
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
    , fileMenuOpen : Bool
    , lastDocId : Maybe String
    }


type alias UserData =
    { email : String
    , language : Translation.Language
    , upgradeModel : Upgrade.Model
    , customer : Maybe String
    , shortcutTrayOpen : Bool
    , documents : DocList.Model
    }


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


lastDocId : Session -> Maybe String
lastDocId session =
    getFromSession .lastDocId session


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


customer : Session -> Maybe String
customer session =
    case session of
        LoggedIn _ data ->
            data.customer

        Guest _ _ ->
            Nothing


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
        settingsDecoder : Decoder { language : Language, customer : Maybe String }
        settingsDecoder =
            Dec.succeed (\lang cust_ -> { language = lang, customer = cust_ })
                |> optional "language" languageDecoder En
                |> optional "customer" (Dec.maybe Dec.string) Nothing
    in
    case ( Dec.decodeValue settingsDecoder json, session ) of
        ( Ok newSettings, LoggedIn sessData userData ) ->
            LoggedIn sessData { userData | language = newSettings.language, customer = newSettings.customer }

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
            Guest (SessionData key errToSeed False Nothing) (GuestData En)


decoder : Nav.Key -> Dec.Decoder Session
decoder key =
    Dec.oneOf [ decodeLoggedIn key, decodeGuest key ]


decodeLoggedIn : Nav.Key -> Dec.Decoder Session
decodeLoggedIn key =
    Dec.succeed
        (\email s lang cust_ trayOpen lastDoc ->
            LoggedIn (SessionData key s False lastDoc) (UserData email lang Upgrade.init cust_ trayOpen DocList.init)
        )
        |> required "email" Dec.string
        |> required "seed" (Dec.int |> Dec.map Random.initialSeed)
        |> optional "language" (Dec.string |> Dec.map langFromString) En
        |> optional "customer" (Dec.maybe Dec.string) Nothing
        |> optional "shortcutTrayOpen" Dec.bool True
        |> optional "lastDocId" (Dec.maybe Dec.string) Nothing


decodeGuest : Nav.Key -> Dec.Decoder Session
decodeGuest key =
    Dec.map2 (\s l -> Guest (SessionData key s False Nothing) (GuestData l))
        (Dec.field "seed" (Dec.int |> Dec.map Random.initialSeed))
        (Dec.field "language" (Dec.string |> Dec.map langFromString))


responseDecoder : Session -> Dec.Decoder Session
responseDecoder session =
    let
        builder email lang cust_ trayOpen =
            case session of
                Guest sessionData data ->
                    LoggedIn sessionData (UserData email lang Upgrade.init cust_ trayOpen DocList.init)

                LoggedIn _ _ ->
                    session
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optionalAt [ "settings", "language" ] (Dec.map langFromString Dec.string) En
        |> optionalAt [ "settings", "customer" ] (Dec.maybe Dec.string) Nothing
        |> optionalAt [ "settings", "shortcutTrayOpen" ] Dec.bool True


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
