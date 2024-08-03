port module Session exposing (Guest, LoggedIn, PaymentStatus(..), Session(..), UserSource(..), confirmEmail, copyNaming, daysLeft, decode, documents, endFirstRun, features, fileMenuOpen, fromLegacy, getDocName, getMetadata, isFirstRun, isNotConfirmed, isOwner, lastDocId, logout, name, paymentStatus, public, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, setFileOpen, setShortcutTrayOpen, setSortBy, shortcutTrayOpen, sortBy, storeLogin, storeSignup, sync, toGuest, updateDocuments, updateUpgrade, upgradeModel, userLoggedIn, userLoggedOut, userSettingsChange)

import Coders exposing (sortByDecoder)
import Doc.List as DocList exposing (Model(..))
import Doc.Metadata as Metadata exposing (Metadata)
import Features exposing (Feature)
import Http
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Enc
import List.Extra as ListExtra
import Outgoing exposing (Msg(..), send)
import Regex
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
    , firstRun : Bool
    }


type alias UserData =
    { email : String
    , upgradeModel : Upgrade.Model
    , paymentStatus : PaymentStatus
    , confirmedAt : Maybe Time.Posix
    , shortcutTrayOpen : Bool
    , sortBy : SortBy
    , documents : DocList.Model
    , features : List Feature
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


isFirstRun : LoggedIn -> Bool
isFirstRun (LoggedIn sessionData _) =
    sessionData.firstRun


endFirstRun : LoggedIn -> LoggedIn
endFirstRun (LoggedIn sessionData userData) =
    LoggedIn { sessionData | firstRun = False } userData


fileMenuOpen : LoggedIn -> Bool
fileMenuOpen session =
    getFromLoggedInSession .fileMenuOpen session


upgradeModel : LoggedIn -> Upgrade.Model
upgradeModel (LoggedIn _ data) =
    data.upgradeModel


paymentStatus : LoggedIn -> PaymentStatus
paymentStatus (LoggedIn _ userData) =
    userData.paymentStatus


features : LoggedIn -> List Feature
features (LoggedIn _ userData) =
    userData.features


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


copyNaming : LoggedIn -> String -> String
copyNaming (LoggedIn _ data) originalName =
    let
        copyNameRegex =
            Maybe.withDefault Regex.never <|
                Regex.fromString (originalName ++ "\\s*(\\(\\d+\\))?$")
    in
    case data.documents of
        Success docList ->
            docList
                |> List.filter
                    (\d ->
                        d
                            |> Metadata.getDocName
                            |> Maybe.withDefault ""
                            |> Regex.find copyNameRegex
                            |> (not << List.isEmpty)
                    )
                |> List.length
                |> (\n ->
                        if n > 0 then
                            originalName ++ " (" ++ String.fromInt (n + 1) ++ ")"

                        else
                            originalName
                   )

        _ ->
            originalName


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


isOwner : LoggedIn -> String -> Bool
isOwner session docId =
    getMetadata session docId
        |> Maybe.map Metadata.getCollaborators
        |> Maybe.map (not << List.member (name session))
        |> Maybe.withDefault False



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


public : Session
public =
    GuestSession
        (Guest
            { fileMenuOpen = False
            , lastDocId = Nothing
            , fromLegacy = False
            , firstRun = False
            }
        )


decode : Dec.Value -> Session
decode json =
    case Dec.decodeValue decoderLoggedIn json of
        Ok session ->
            LoggedInSession session

        Err _ ->
            case Dec.decodeValue decoderGuestSession json of
                Ok session ->
                    GuestSession session

                Err _ ->
                    GuestSession
                        (Guest
                            { fileMenuOpen = False
                            , lastDocId = Nothing
                            , fromLegacy = False
                            , firstRun = False
                            }
                        )


decoderGuestSession : Dec.Decoder Guest
decoderGuestSession =
    Dec.succeed
        (\legacy side ->
            Guest
                { fileMenuOpen = side
                , lastDocId = Nothing
                , fromLegacy = legacy
                , firstRun = False
                }
        )
        |> optional "fromLegacy" Dec.bool False
        |> optional "sidebarOpen" Dec.bool False


decoderLoggedIn : Dec.Decoder LoggedIn
decoderLoggedIn =
    Dec.succeed
        (\email t legacy side confirmTime payStat trayOpen sortCriteria _ featList ->
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
                , firstRun = False
                }
                (UserData email Upgrade.init newPayStat confirmTime trayOpen sortCriteria DocList.init featList)
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
        |> optional "features" Features.decoder []


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

                    other ->
                        Dec.fail ("Invalid payment status:[" ++ String.join "," other ++ "]")
            )


type UserSource
    = FromSignup
    | Other


responseDecoder : UserSource -> Guest -> Dec.Decoder ( LoggedIn, Language )
responseDecoder usrSrc session =
    let
        sessionData =
            guestSessionData session
                |> (\data ->
                        case usrSrc of
                            FromSignup ->
                                { data | firstRun = True }

                            _ ->
                                data
                   )

        builder : String -> PaymentStatus -> Maybe Time.Posix -> Language -> List Metadata -> List Feature -> ( LoggedIn, Language )
        builder email payStat confAt lang docs feats =
            ( LoggedIn
                sessionData
                (UserData email Upgrade.init payStat confAt True ModifiedAt (DocList.fromList docs) feats)
            , lang
            )
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optional "paymentStatus" decodePaymentStatus (Trial (Time.millisToPosix 0))
        |> optional "confirmedAt" decodeConfirmedStatus (Just (Time.millisToPosix 0))
        |> optional "language" (Dec.string |> Dec.map Translation.langFromString) Translation.En
        |> optional "documents" Metadata.responseDecoder []
        |> optional "features" Features.decoder []


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
        , expect = Http.expectJson toMsg (responseDecoder FromSignup session)
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
        , expect = Http.expectJson toMsg (responseDecoder Other session)
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
        , expect = Http.expectJson toMsg (responseDecoder Other session)
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
        , expect = Http.expectJson toMsg (responseDecoder Other session)
        }


logout : Cmd msg
logout =
    send <| LogoutUser


toGuest : LoggedIn -> Guest
toGuest (LoggedIn sessionData _) =
    Guest sessionData



-- PORTS


store : Translation.Language -> LoggedIn -> Cmd msg
store lang session =
    send <| StoreUser (encode lang session)


userLoggedIn : msg -> Sub msg
userLoggedIn toMsg =
    userLoggedInMsg (always toMsg)


userLoggedOut : msg -> Sub msg
userLoggedOut toMsg =
    userLoggedOutMsg (always toMsg)


port userLoggedInMsg : (() -> msg) -> Sub msg


port userLoggedOutMsg : (() -> msg) -> Sub msg


port userSettingsChange : (Dec.Value -> msg) -> Sub msg
