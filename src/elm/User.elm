port module User exposing (User, db, decode, language, loggedIn, loginChanges, logout, name, navKey, requestLogin, requestSignup, seed, setLanguage, settingsChange, storeLogin, storeSignup)

import Browser.Navigation as Nav
import Http
import Json.Decode as Dec
import Json.Decode.Pipeline exposing (optionalAt, required)
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Translation exposing (Language(..), langFromString, langToString, languageDecoder)
import Utils exposing (hexEncode)



-- MODEL


type User
    = LoggedIn Nav.Key UserData
    | Guest Nav.Key GuestData


type alias UserData =
    { email : String
    , seed : Int
    , language : Translation.Language
    }


type alias GuestData =
    { seed : Int
    , language : Translation.Language
    }



-- GETTERS


navKey : User -> Nav.Key
navKey user =
    case user of
        LoggedIn key _ ->
            key

        Guest key _ ->
            key


name : User -> Maybe String
name user =
    case user of
        LoggedIn _ { email } ->
            Just email

        Guest _ _ ->
            Nothing


seed : User -> Int
seed user =
    case user of
        LoggedIn _ data ->
            data.seed

        Guest _ data ->
            data.seed


db : User -> Maybe String
db user =
    case user of
        LoggedIn _ { email } ->
            Just ("userdb-" ++ hexEncode email)

        Guest _ _ ->
            Nothing


language : User -> Language
language user =
    case user of
        LoggedIn _ data ->
            data.language

        Guest _ data ->
            data.language


loggedIn : User -> Bool
loggedIn user =
    case user of
        LoggedIn _ _ ->
            True

        Guest _ _ ->
            False



-- UPDATE


setLanguage : Language -> User -> User
setLanguage lang user =
    case user of
        LoggedIn key data ->
            LoggedIn key { data | language = lang }

        Guest key data ->
            Guest key { data | language = lang }



-- ENCODER & DECODER


decode : Nav.Key -> Dec.Value -> User
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
            in
            Guest key (GuestData errToSeed En)


decoder : Nav.Key -> Dec.Decoder User
decoder key =
    Dec.oneOf [ decodeLoggedIn key, decodeGuest key ]


decodeLoggedIn : Nav.Key -> Dec.Decoder User
decodeLoggedIn key =
    Dec.map3 UserData
        (Dec.field "email" Dec.string)
        (Dec.field "seed" Dec.int)
        (Dec.field "language" (Dec.string |> Dec.map langFromString))
        |> Dec.map (LoggedIn key)


decodeGuest : Nav.Key -> Dec.Decoder User
decodeGuest key =
    Dec.map2 GuestData
        (Dec.field "seed" Dec.int)
        (Dec.field "language" (Dec.string |> Dec.map langFromString))
        |> Dec.map (Guest key)


responseDecoder : User -> Dec.Decoder User
responseDecoder user =
    let
        builder email lang =
            case user of
                Guest key data ->
                    LoggedIn key (UserData email data.seed lang)

                LoggedIn _ _ ->
                    user
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optionalAt [ "settings", "language" ] (Dec.map langFromString Dec.string) En


encode : User -> Enc.Value
encode user =
    case user of
        LoggedIn _ data ->
            Enc.object
                [ ( "email", Enc.string data.email )
                , ( "language", data.language |> langToString |> Enc.string )
                , ( "seed", Enc.int data.seed )
                ]

        Guest _ data ->
            Enc.object
                [ ( "seed", Enc.int data.seed )
                , ( "language", data.language |> langToString |> Enc.string )
                ]



-- AUTHENTICATION


requestSignup : (Result Http.Error User -> msg) -> String -> String -> User -> Cmd msg
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


storeSignup : User -> Cmd msg
storeSignup user =
    store (Just user)


requestLogin : (Result Http.Error User -> msg) -> String -> String -> User -> Cmd msg
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


storeLogin : User -> Cmd msg
storeLogin user =
    store (Just user)


logout : User -> ( User, Cmd msg )
logout user =
    case user of
        LoggedIn key data ->
            ( Guest key (GuestData data.seed data.language)
            , store Nothing
            )

        Guest _ _ ->
            ( user, Cmd.none )



-- PORTS


store : Maybe User -> Cmd msg
store user_ =
    case user_ of
        Just user ->
            send <| StoreUser (encode user)

        Nothing ->
            send <| StoreUser Enc.null


loginChanges : (User -> msg) -> Nav.Key -> Sub msg
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
