port module Session exposing (PaymentStatus(..), Session, confirmEmail, daysLeft, decode, documents, fileMenuOpen, fromLegacy, getDocName, getMetadata, isNotConfirmed, lastDocId, loggedIn, loginChanges, logout, name, paymentStatus, requestForgotPassword, requestLogin, requestResetPassword, requestSignup, setFileOpen, setShortcutTrayOpen, setSortBy, shortcutTrayOpen, sortBy, storeLogin, storeSignup, sync, updateDocuments, updateUpgrade, upgradeModel, userSettingsChange)

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
    = LoggedIn SessionData UserData
    | Guest SessionData


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


getFromSession : (SessionData -> a) -> Session -> a
getFromSession getter session =
    case session of
        LoggedIn sessionData _ ->
            getter sessionData

        Guest sessionData ->
            getter sessionData


name : Session -> Maybe String
name session =
    case session of
        LoggedIn _ { email } ->
            Just email

        Guest _ ->
            Nothing


lastDocId : Session -> Maybe String
lastDocId session =
    getFromSession .lastDocId session


fromLegacy : Session -> Bool
fromLegacy session =
    getFromSession .fromLegacy session


fileMenuOpen : Session -> Bool
fileMenuOpen session =
    getFromSession .fileMenuOpen session


upgradeModel : Session -> Maybe Upgrade.Model
upgradeModel session =
    case session of
        LoggedIn _ data ->
            Just data.upgradeModel

        Guest _ ->
            Nothing


paymentStatus : Time.Posix -> Session -> PaymentStatus
paymentStatus cTime session =
    case session of
        LoggedIn _ data ->
            data.paymentStatus

        Guest _ ->
            Trial (cTime |> add14days)


add14days : Posix -> Posix
add14days time =
    Time.posixToMillis time
        |> (\ms -> 3600 * 24 * 1000 * 14 + ms |> Time.millisToPosix)


daysLeft : Time.Posix -> Session -> Maybe Int
daysLeft cTime session =
    case paymentStatus cTime session of
        Trial expiry ->
            ((Time.posixToMillis expiry - Time.posixToMillis cTime) |> toFloat)
                / (1000 * 3600 * 24)
                |> round
                |> Just

        Customer _ ->
            Nothing


isNotConfirmed : Session -> Bool
isNotConfirmed session =
    case session of
        LoggedIn _ data ->
            data.confirmedAt == Nothing

        Guest _ ->
            True


shortcutTrayOpen : Session -> Bool
shortcutTrayOpen session =
    case session of
        LoggedIn _ data ->
            data.shortcutTrayOpen

        Guest _ ->
            False


sortBy : Session -> SortBy
sortBy session =
    case session of
        LoggedIn _ data ->
            data.sortBy

        Guest _ ->
            ModifiedAt


documents : Session -> DocList.Model
documents session =
    case session of
        LoggedIn _ data ->
            data.documents

        Guest _ ->
            DocList.init


getMetadata : Session -> String -> Maybe Metadata
getMetadata session docId =
    case documents session of
        Success docList ->
            docList
                |> ListExtra.find (\d -> Metadata.getDocId d == docId)

        _ ->
            Nothing


getDocName : Session -> String -> Maybe String
getDocName session docId =
    getMetadata session docId
        |> Maybe.andThen Metadata.getDocName


loggedIn : Session -> Bool
loggedIn session =
    case session of
        LoggedIn _ _ ->
            True

        Guest _ ->
            False



-- UPDATE


sync : Dec.Value -> Time.Posix -> Session -> Session
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

        ( Ok newSettings, Guest sessData ) ->
            Guest sessData

        ( Err _, _ ) ->
            session


updateSession : (SessionData -> SessionData) -> Session -> Session
updateSession updateFn session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn (updateFn sessionData) data

        Guest sessionData ->
            Guest (updateFn sessionData)


setFileOpen : Bool -> Session -> Session
setFileOpen isOpen session =
    updateSession (\s -> { s | fileMenuOpen = isOpen }) session


confirmEmail : Time.Posix -> Session -> Session
confirmEmail currentTime session =
    case session of
        LoggedIn key data ->
            LoggedIn key { data | confirmedAt = Just currentTime }

        Guest _ ->
            session


setShortcutTrayOpen : Bool -> Session -> Session
setShortcutTrayOpen isOpen session =
    case session of
        LoggedIn key data ->
            LoggedIn key { data | shortcutTrayOpen = isOpen }

        Guest _ ->
            session


setSortBy : SortBy -> Session -> Session
setSortBy newSort session =
    case session of
        LoggedIn key data ->
            LoggedIn key { data | sortBy = newSort }

        Guest _ ->
            session


updateDocuments : DocList.Model -> Session -> Session
updateDocuments docList session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn sessionData { data | documents = DocList.update data.sortBy docList data.documents }

        Guest _ ->
            session


updateUpgrade : Upgrade.Msg -> Session -> Session
updateUpgrade upgradeMsg session =
    case session of
        LoggedIn sessionData data ->
            LoggedIn sessionData { data | upgradeModel = Upgrade.update upgradeMsg data.upgradeModel }

        Guest _ ->
            session



-- ENCODER & DECODER


decode : Dec.Value -> Session
decode json =
    case Dec.decodeValue decoder json of
        Ok session ->
            session

        Err err ->
            Guest
                { fileMenuOpen = False
                , lastDocId = Nothing
                , fromLegacy = False
                }


decoder : Dec.Decoder Session
decoder =
    Dec.oneOf [ decodeLoggedIn, decodeGuest ]


decodeLoggedIn : Dec.Decoder Session
decodeLoggedIn =
    Dec.succeed
        (\email t legacy side confirmTime trialExp_ custId_ trayOpen sortCriteria lastDoc ->
            let
                newPayStat =
                    toPayStat t trialExp_ custId_
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
        |> optional "trialExpiry" decodeTrialExpiry Nothing
        |> optional "customerId" decodeCustomerId Nothing
        |> optional "shortcutTrayOpen" Dec.bool False
        |> optional "sortBy" sortByDecoder ModifiedAt
        |> optional "lastDocId" (Dec.maybe Dec.string) Nothing


decodeConfirmedStatus : Decoder (Maybe Time.Posix)
decodeConfirmedStatus =
    Dec.oneOf
        [ Dec.null Nothing
        , Dec.int |> Dec.map Time.millisToPosix |> Dec.maybe
        ]


decodeTrialExpiry : Decoder (Maybe Time.Posix)
decodeTrialExpiry =
    Dec.oneOf
        [ Dec.null Nothing
        , Dec.int |> Dec.map Time.millisToPosix |> Dec.maybe
        ]


decodeCustomerId : Decoder (Maybe String)
decodeCustomerId =
    Dec.oneOf
        [ Dec.null Nothing
        , Dec.string |> Dec.maybe
        ]


toPayStat : Time.Posix -> Maybe Time.Posix -> Maybe String -> PaymentStatus
toPayStat t trialExp_ custId_ =
    case ( trialExp_, custId_ ) of
        ( Just trialExp, Nothing ) ->
            if trialExp == Time.millisToPosix 0 then
                Trial (t |> add14days)

            else
                Trial trialExp

        ( Nothing, Just custId ) ->
            Customer custId

        _ ->
            Trial (t |> add14days)


decodePaymentStatus : Dec.Decoder PaymentStatus
decodePaymentStatus =
    Dec.oneOf
        [ Dec.succeed Customer |> required "customer" Dec.string
        , Dec.succeed Trial |> required "trialExpires" (Dec.int |> Dec.map Time.millisToPosix)
        ]


decodeGuest : Dec.Decoder Session
decodeGuest =
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


responseDecoder : Session -> Dec.Decoder ( Session, Language )
responseDecoder session =
    let
        builder : String -> Maybe Time.Posix -> Maybe String -> Maybe Time.Posix -> Language -> ( Session, Language )
        builder email trialExp_ custId_ confAt lang =
            case session of
                Guest sessionData ->
                    ( LoggedIn sessionData (UserData email Upgrade.init (toPayStat (Time.millisToPosix 0) trialExp_ custId_) confAt True ModifiedAt DocList.init)
                    , lang
                    )

                LoggedIn _ _ ->
                    ( session, lang )
    in
    Dec.succeed builder
        |> required "email" Dec.string
        |> optional "trialExpiry" decodeTrialExpiry Nothing
        |> optional "customerId" decodeCustomerId Nothing
        |> optional "confirmedAt" decodeConfirmedStatus (Just (Time.millisToPosix 0))
        |> optional "language" (Dec.string |> Dec.map Translation.langFromString) Translation.En


encode : Translation.Language -> Session -> Enc.Value
encode lang session =
    case session of
        LoggedIn _ data ->
            Enc.object
                [ ( "email", Enc.string data.email )
                , ( "language", Enc.string (Translation.langToString lang) )
                ]

        Guest _ ->
            Enc.null



-- AUTHENTICATION


requestSignup : (Result Http.Error ( Session, Language ) -> msg) -> String -> String -> Bool -> Session -> Cmd msg
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


storeSignup : Translation.Language -> Session -> Cmd msg
storeSignup lang session =
    store lang session


requestLogin : (Result Http.Error ( Session, Language ) -> msg) -> String -> String -> Session -> Cmd msg
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


storeLogin : Translation.Language -> Session -> Cmd msg
storeLogin lang session =
    store lang session


requestForgotPassword : (Result Http.Error ( Session, Language ) -> msg) -> String -> Session -> Cmd msg
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


requestResetPassword : (Result Http.Error ( Session, Language ) -> msg) -> { newPassword : String, token : String } -> Session -> Cmd msg
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


store : Translation.Language -> Session -> Cmd msg
store lang session =
    send <| StoreUser (encode lang session)


loginChanges : (Session -> msg) -> Sub msg
loginChanges toMsg =
    userLoginChange (decode >> toMsg)


port userLoginChange : (Dec.Value -> msg) -> Sub msg


port userSettingsChange : (Dec.Value -> msg) -> Sub msg
