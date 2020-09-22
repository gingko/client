port module User exposing (User, changes, db, decode, language, loggedIn, logout, name, navKey, requestLogin, requestSignup, seed, setLanguage, storeLogin, storeSignup)

import Browser.Navigation as Nav
import Http
import Json.Decode as Dec
import Json.Encode as Enc
import Outgoing exposing (Msg(..), send)
import Translation exposing (Language, langFromString, langToString)
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


login : User -> String -> User
login user email =
    case user of
        Guest key data ->
            LoggedIn key (UserData email data.seed data.language)

        LoggedIn _ _ ->
            user



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
                        |> Debug.log "decode error"
                        |> String.left 5
                        |> String.toList
                        |> List.map Char.toCode
                        |> List.foldl (+) 12345
            in
            Guest key (GuestData errToSeed Translation.En)


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

        responseDecoder =
            Dec.map (login user)
                (Dec.field "email" Dec.string)
    in
    Http.post
        { url = "/signup"
        , body = requestBody
        , expect = Http.expectJson toMsg responseDecoder
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

        responseDecoder =
            Dec.map (login user)
                (Dec.field "email" Dec.string)
    in
    Http.riskyRequest
        { method = "POST"
        , url = "/login"
        , headers = []
        , body = requestBody
        , expect = Http.expectJson toMsg responseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


storeLogin : User -> Cmd msg
storeLogin user =
    store (Just user)


logout : Cmd msg
logout =
    store Nothing



-- PORTS


store : Maybe User -> Cmd msg
store user_ =
    case user_ of
        Just user ->
            send <| StoreUser (encode user)

        Nothing ->
            send <| StoreUser Enc.null


changes : (User -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    userStateChanged (decode key >> toMsg)


port userStateChanged : (Dec.Value -> msg) -> Sub msg
