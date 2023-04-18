port module Session exposing (Guest, LoggedIn, PaymentStatus(..), Session(..), confirmEmail, daysLeft, decode, documents, fileMenuOpen, fromLegacy, getDocName, getMetadata, isNotConfirmed, lastDocId, logout, name, paymentStatus, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, setFileOpen, setShortcutTrayOpen, setSortBy, shortcutTrayOpen, sortBy, storeLogin, storeSignup, sync, updateDocuments, updateUpgrade, upgradeModel, userLoggedIn, userSettingsChange)

import Coders exposing (sortByDecoder)
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata exposing (Metadata)
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Enc
import List.Extra as ListExtra
import Outgoing exposing (Msg(..), send)
import Time exposing (Posix)
import Translation exposing (Language)
import Types exposing (SortBy(..))
import Upgrade



-- MODEL


type Session
    = GuestSession Guest
    | LoggedInSession LoggedIn


type Guest
    = Guest SessionData


type LoggedIn
    = LoggedIn SessionData UserData


type alias SessionData =
    -- Not persisted
    { fileMenuOpen : Bool
    , lastDocId : Maybe String
    , fromLegacy : Bool
    }


type alias UserData =
    { email : String
    , upgradeModel : Upgrade.Model
    , paymentStatus : PaymentStatus
    , confirmedAt : Maybe Time.Posix
    , shortcutTrayOpen : Bool
    , sortBy : SortBy
    , documents : DocList.Model
    }


type PaymentStatus
    = Trial Time.Posix
    | Customer String



-- GETTERS


guestSessionData : Guest -> SessionData
guestSessionData (Guest sessionData) =
    sessionData


getFromLoggedInSession : (SessionData -> a) -> LoggedIn -> a
getFromLoggedInSession getter (LoggedIn sessionData _) =
    getter sessionData


name : LoggedIn -> String
name (LoggedIn _ { email }) =
    email


lastDocId : LoggedIn -> Maybe String
lastDocId session =
    getFromLoggedInSession .lastDocId session


fromLegacy : Guest -> Bool
fromLegacy (Guest sessionData) =
    sessionData.fromLegacy


fileMenuOpen : LoggedIn -> Bool
fileMenuOpen session =
    getFromLoggedInSession .fileMenuOpen session


upgradeModel : LoggedIn -> Upgrade.Model
upgradeModel (LoggedIn _ data) =
    data.upgradeModel


paymentStatus : LoggedIn -> PaymentStatus
paymentStatus (LoggedIn _ userData) =
    userData.paymentStatus


add14days : Posix -> Posix
add14days time =
    Time.posixToMillis time
        |> (\ms -> 3600 * 24 * 1000 * 14 + ms |> Time.millisToPosix)


daysLeft : Time.Posix -> LoggedIn -> Maybe Int
daysLeft cTime session =
    case paymentStatus session of
        Trial expiry ->
            ((Time.posixToMillis expiry - Time.posixToMillis cTime) |> toFloat)
                / (1000 * 3600 * 24)
                |> round
                |> Just

        Customer _ ->
            Nothing


isNotConfirmed : LoggedIn -> Bool
isNotConfirmed (LoggedIn _ data) =
    data.confirmedAt == Nothing


shortcutTrayOpen : LoggedIn -> Bool
shortcutTrayOpen (LoggedIn _ data) =
    data.shortcutTrayOpen


sortBy : LoggedIn -> SortBy
sortBy (LoggedIn _ data) =
    data.sortBy


documents : LoggedIn -> DocList.Model
documents (LoggedIn _ data) =
    data.documents


getMetadata : LoggedIn -> String -> Maybe Metadata
getMetadata session docId =
    case documents session of
        Success docList ->
            docList
                |> ListExtra.find (\d -> Metadata.getDocId d == docId)

        _ ->
            Nothing


getDocName : LoggedIn -> String -> Maybe String
getDocName session docId =
    getMetadata session docId
        |> Maybe.andThen Metadata.getDocName



-- UPDATE


sync : Dec.Value -> Time.Posix -> LoggedIn -> LoggedIn
sync json currentTime session =
    let
        settingsDecoder : Decoder { paymentStatus : PaymentStatus, confirmedAt : Maybe Time.Posix }
        settingsDecoder =
            Dec.succeed (\payStat confAt -> { paymentStatus = payStat, confirmedAt = confAt })
                |> optional "paymentStatus" decodePaymentStatus (Trial (currentTime |> add14days))
                |> optional "confirmedAt" decodeConfirmedStatus (Just (Time.millisToPosix 0))
    in
    case ( Dec.decodeValue settingsDecoder json, session ) of
        ( Ok newSettings, LoggedIn sessData userData ) ->
            LoggedIn sessData
                { userData
                    | paymentStatus = newSettings.paymentStatus
                    , confirmedAt = newSettings.confirmedAt
                }

        ( Err _, _ ) ->
            session


setFileOpen : Bool -> LoggedIn -> LoggedIn
setFileOpen isOpen (LoggedIn sessData userData) =
    LoggedIn { sessData | fileMenuOpen = isOpen } userData


confirmEmail : Time.Posix -> LoggedIn -> LoggedIn
confirmEmail currentTime (LoggedIn key data) =
    LoggedIn key { data | confirmedAt = Just currentTime }


setShortcutTrayOpen : Bool -> LoggedIn -> LoggedIn
setShortcutTrayOpen isOpen (LoggedIn key data) =
    LoggedIn key { data | shortcutTrayOpen = isOpen }


setSortBy : SortBy -> LoggedIn -> LoggedIn
setSortBy newSort (LoggedIn sessData userData) =
    LoggedIn sessData { userData | sortBy = newSort }


updateDocuments : DocList.Model -> LoggedIn -> LoggedIn
updateDocuments docList (LoggedIn sessData userData) =
    LoggedIn sessData { userData | documents = DocList.update userData.sortBy docList userData.documents }


updateUpgrade : Upgrade.Msg -> LoggedIn -> LoggedIn
updateUpgrade upgradeMsg (LoggedIn sessionData data) =
    LoggedIn sessionData { data | upgradeModel = Upgrade.update upgradeMsg data.upgradeModel }



-- ENCODER & DECODER


decode : Dec.Value -> Session
decode json =
    case Dec.decodeValue decoderLoggedIn json of
        Ok session ->
            LoggedInSession session

        Err err ->
            case Dec.decodeValue decoderGuestSession json of
                Ok session ->
                    GuestSession session

                Err err2 ->
                    GuestSession
                        (Guest
                            { fileMenuOpen = False
                            , lastDocId = Nothing
                            , fromLegacy = False
                            }
                        )


decodeLoggedIn : Dec.Value -> LoggedIn
decodeLoggedIn json =
    case Dec.decodeValue decoderLoggedIn json of
        Ok session ->
            session

        Err err ->
            LoggedIn
                { fileMenuOpen = False
                , lastDocId = Nothing
                , fromLegacy = False
                }
                (UserData "" Upgrade.init (Trial (Time.millisToPosix 0)) Nothing False ModifiedAt DocList.init)


decoderGuestSession : Dec.Decoder Guest
decoderGuestSession =
    Dec.succeed
        (\legacy side ->
            Guest
                { fileMenuOpen = side
                , lastDocId = Nothing
                , fromLegacy = legacy
                }
        )
        |> optional "fromLegacy" Dec.bool False
        |> optional "sidebarOpen" Dec.bool False


decoderLoggedIn : Dec.Decoder LoggedIn
decoderLoggedIn =
    Dec.succeed
        (\email t legacy side confirmTime payStat trayOpen sortCriteria lastDoc ->
            let
                newPayStat =
                    case payStat of
                        Trial trialTime ->
                            if Time.posixToMillis trialTime == 0 then
                                Trial (t |> add14days)

                            else
                                payStat

                        _ ->
                            payStat
            in
            LoggedIn
                { fileMenuOpen = side
                , lastDocId = Nothing
                , fromLegacy = legacy
                }
                (UserData email Upgrade.init newPayStat confirmTime trayOpen sortCriteria DocList.init)
        )
        |> required "email" Dec.string
        |> required "currentTime" (Dec.int |> Dec.map Time.millisToPosix)
        |> optional "fromLegacy" Dec.bool False
        |> optional "sidebarOpen" Dec.bool False
        |> optional "confirmedAt" decodeConfirmedStatus (Just (Time.millisToPosix 0))
        |> optional "paymentStatus" decodePaymentStatus (Trial (Time.millisToPosix 0))
        |> optional "shortcutTrayOpen" Dec.bool False
        |> optional "sortBy" sortByDecoder ModifiedAt
        |> optional "lastDocId" (Dec.maybe Dec.string) Nothing


decodeConfirmedStatus : Decoder (Maybe Time.Posix)
decodeConfirmedStatus =
    Dec.oneOf
        [ Dec.null Nothing
        , Dec.int |> Dec.map Time.millisToPosix |> Dec.maybe
        ]


decodePaymentStatus : Dec.Decoder PaymentStatus
decodePaymentStatus =
    Dec.string
        |> Dec.andThen
            (\str ->
                case str |> String.split ":" of
                    "trial" :: valString :: [] ->
                        Trial (String.toInt valString |> Maybe.withDefault 0 |> Time.millisToPosix) |> Dec.succeed

                    "customer" :: custId :: [] ->
                        Customer custId |> Dec.succeed

                    _ ->
                        Dec.fail "Invalid payment status"
            )


responseDecoder : Guest -> Dec.Decoder ( LoggedIn, Language )
responseDecoder session =
    let
        sessionData =
            guestSessionData session

        builder : String -> PaymentStatus -> Maybe Time.Posix -> Language -> List Metadata -> ( LoggedIn, Language )
        builder email payStat confAt lang docs =
            ( LoggedIn sessionData (UserData email Upgrade.init payStat confAt True ModifiedAt (DocList.fromList docs))
            , lang
            )
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optional "paymentStatus" decodePaymentStatus (Trial (Time.millisToPosix 0))
        |> optional "confirmedAt" decodeConfirmedStatus (Just (Time.millisToPosix 0))
        |> optional "language" (Dec.string |> Dec.map Translation.langFromString) Translation.En
        |> optional "documents" Metadata.responseDecoder []


encodeUserData : Language -> UserData -> Enc.Value
encodeUserData lang userData =
    Enc.object
        [ ( "email", Enc.string userData.email )
        , ( "paymentStatus", encodePaymentStatus userData.paymentStatus )
        , ( "confirmedAt", userData.confirmedAt |> Maybe.map Time.posixToMillis |> Coders.maybeToValue Enc.int )
        , ( "shortcutTrayOpen", Enc.bool userData.shortcutTrayOpen )
        , ( "sortBy", Coders.sortByEncoder userData.sortBy )
        , ( "language", Enc.string (Translation.langToString lang) )
        ]


encodePaymentStatus : PaymentStatus -> Enc.Value
encodePaymentStatus payStat =
    case payStat of
        Trial trialTime ->
            Enc.string ("trial:" ++ (trialTime |> Time.posixToMillis |> String.fromInt))

        Customer custId ->
            Enc.string ("customer:" ++ custId)


encode : Translation.Language -> LoggedIn -> Enc.Value
encode lang (LoggedIn _ userData) =
    encodeUserData lang userData



-- AUTHENTICATION


requestSignup : (Result Http.Error ( LoggedIn, Language ) -> msg) -> String -> String -> Bool -> Guest -> Cmd msg
requestSignup toMsg email password didOptIn session =
    let
        requestBody =
            Enc.object
                [ ( "email", Enc.string email )
                , ( "password", Enc.string password )
                , ( "subscribed", Enc.bool didOptIn )
                ]
                |> Http.jsonBody
    in
    Http.post
        { url = "/signup"
        , body = requestBody
        , expect = Http.expectJson toMsg (responseDecoder session)
        }


storeSignup : Translation.Language -> LoggedIn -> Cmd msg
storeSignup lang session =
    store lang session


requestLogin : (Result Http.Error ( LoggedIn, Language ) -> msg) -> String -> String -> Guest -> Cmd msg
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


storeLogin : Translation.Language -> LoggedIn -> Cmd msg
storeLogin lang session =
    store lang session


requestForgotPassword : (Result Http.Error ( LoggedIn, Language ) -> msg) -> String -> Guest -> Cmd msg
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


requestResetPassword : (Result Http.Error ( LoggedIn, Language ) -> msg) -> { newPassword : String, token : String } -> Guest -> Cmd msg
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
    send <| LogoutUser



-- PORTS


store : Translation.Language -> LoggedIn -> Cmd msg
store lang session =
    send <| StoreUser (encode lang session)


userLoggedIn : msg -> Sub msg
userLoggedIn toMsg =
    userLoggedInMsg (always toMsg)


port userLoggedInMsg : (() -> msg) -> Sub msg


port userSettingsChange : (Dec.Value -> msg) -> Sub msg
