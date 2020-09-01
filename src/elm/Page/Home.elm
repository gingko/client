module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Import
import Json.Decode as Dec
import Ports exposing (IncomingMsg(..), OutgoingMsg(..), receiveMsg, sendOut)
import RandomId
import Route
import Session exposing (Session)
import Task
import Time
import Translation exposing (langFromString)



-- MODEL


type alias Model =
    { documents : List Metadata
    , language : Translation.Language
    , session : Session
    , isImporting : Bool
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( { documents = [], language = langFromString "en", session = session, isImporting = False }
    , getDocumentList session
    )


toSession : Model -> Session
toSession model =
    model.session



-- UPDATE


type Msg
    = ReceivedDocuments (Result Http.Error (List Metadata))
    | GetNewDocId
    | NewDocIdReceived String
    | DeleteDoc String
    | ImportFileRequested
    | ImportFileSelected File
    | ImportFileLoaded String String
    | Port IncomingMsg
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDocuments (Ok docList) ->
            let
                sortedList =
                    docList
                        |> List.sortBy (Time.posixToMillis << Metadata.getUpdatedAt)
                        |> List.reverse
            in
            ( { model | documents = sortedList }, Cmd.none )

        ReceivedDocuments (Err err) ->
            case err of
                Http.BadStatus 401 ->
                    ( model, Route.replaceUrl (Session.navKey model.session) Route.Login )

                _ ->
                    ( model, Cmd.none )

        GetNewDocId ->
            ( model, RandomId.generate NewDocIdReceived )

        NewDocIdReceived docId ->
            ( model, Route.replaceUrl (Session.navKey model.session) (Route.DocNew docId) )

        DeleteDoc docId ->
            ( model, sendOut <| RequestDelete docId )

        ImportFileRequested ->
            ( model, Select.file [ "text/*", "application/json" ] ImportFileSelected )

        ImportFileSelected file ->
            case Session.username model.session of
                Just username ->
                    ( { model | isImporting = True }, Task.perform (ImportFileLoaded username) (File.toString file) )

                Nothing ->
                    ( { model | isImporting = False }, Cmd.none )

        ImportFileLoaded username contents ->
            case Dec.decodeString (Import.decoder username) contents of
                Ok dataList ->
                    ( { model | isImporting = False }, sendOut <| SaveImportedData (Import.encode dataList) )

                Err err ->
                    let
                        _ =
                            Debug.log "err" err
                    in
                    ( { model | isImporting = False }, Cmd.none )

        Port incomingMsg ->
            case incomingMsg of
                DocListChanged ->
                    ( model, getDocumentList model.session )

                _ ->
                    ( model, Cmd.none )

        LogErr err ->
            ( model
            , sendOut (ConsoleLogRequested err)
            )


getDocumentList : Session -> Cmd Msg
getDocumentList session =
    let
        rowDecoder =
            Dec.field "value" Metadata.decoder

        responseDecoder =
            Dec.field "rows" (Dec.list rowDecoder)
    in
    case Session.userDb session of
        Just userDb ->
            Http.riskyRequest
                { url = "/db/" ++ userDb ++ "/_design/testDocList/_view/docList"
                , method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectJson ReceivedDocuments responseDecoder
                , headers = []
                , timeout = Nothing
                , tracker = Nothing
                }

        Nothing ->
            Cmd.none



-- VIEW


view : Model -> Html Msg
view model =
    if not model.isImporting then
        div []
            [ h1 [] [ text "This is the home page" ]
            , ul [] (List.map viewDocEntry model.documents)
            , button [ onClick GetNewDocId ] [ text "New" ]
            , button [ onClick ImportFileRequested ] [ text "Legacy Import" ]
            ]

    else
        div []
            [ h1 [] [ text "This is the home page" ]
            , ul [] (List.map viewDocEntry model.documents)
            , h1 [] [ text "Importing documents..." ]
            ]


viewDocEntry : Metadata -> Html Msg
viewDocEntry metadata =
    let
        docId =
            Metadata.getDocId metadata

        docName =
            Metadata.getDocName metadata |> Maybe.withDefault "Untitled"
    in
    li [] [ a [ href <| "/" ++ docId ] [ text docName ], button [ onClick <| DeleteDoc docId ] [ text "X" ] ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveMsg Port LogErr
