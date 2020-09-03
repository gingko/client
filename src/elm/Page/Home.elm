module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, button, div, h1, input, li, text, ul)
import Html.Attributes exposing (checked, href, type_)
import Html.Events exposing (onCheck, onClick)
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
import Types exposing (Tree)



-- MODEL


type Model
    = Loading PageData
    | Loaded PageData
    | ImportSelecting
        (List
            { selected : Bool
            , tree : ( String, Metadata, Tree )
            }
        )
        PageData
    | ImportSaving
        (List
            { selected : Bool
            , tree : ( String, Metadata, Tree )
            }
        )
        PageData
    | Error String PageData


type alias PageData =
    { documents : List Metadata
    , language : Translation.Language
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading
        { documents = []
        , language = langFromString "en"
        , session = session
        }
    , getDocumentList session
    )


toSession : Model -> Session
toSession model =
    (getData model).session


getData : Model -> PageData
getData model =
    case model of
        Loading pdata ->
            pdata

        Loaded pdata ->
            pdata

        ImportSelecting _ pdata ->
            pdata

        ImportSaving _ pdata ->
            pdata

        Error _ pdata ->
            pdata



-- UPDATE


type Msg
    = ReceivedDocuments (Result Http.Error (List Metadata))
    | GetNewDocId
    | NewDocIdReceived String
    | DeleteDoc String
    | ImportFileRequested
    | ImportFileSelected File
    | ImportTreeSelected String Bool
    | ImportFileLoaded String String
    | ImportSelectionDone
    | Port IncomingMsg
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ReceivedDocuments (Ok _), Loading pageData ) ->
            updatePageData msg pageData
                |> Tuple.mapFirst Loaded

        ( ReceivedDocuments (Err err), Loading pageData ) ->
            updatePageData msg pageData
                |> Tuple.mapFirst (\pd -> Error "NetworkError" pd)

        ( ImportFileLoaded uname contents, Loaded pageData ) ->
            case Dec.decodeString Import.decoder contents of
                Ok dataList ->
                    let
                        listWithSelectState =
                            dataList |> List.map (\t -> { selected = False, tree = t })
                    in
                    ( ImportSelecting listWithSelectState pageData, Cmd.none )

                Err err ->
                    ( model, Cmd.none )

        ( ImportTreeSelected treeId isSelected, ImportSelecting selectList pageData ) ->
            let
                mapFn ({ selected, tree } as orig) =
                    let
                        ( tid, _, _ ) =
                            tree
                    in
                    if tid == treeId then
                        { orig | selected = isSelected }

                    else
                        orig

                newList =
                    selectList |> List.map mapFn
            in
            ( ImportSelecting newList pageData, Cmd.none )

        ( ImportSelectionDone, ImportSelecting selectList pageData ) ->
            let
                author =
                    toSession model |> Session.username |> Maybe.withDefault "jane.doe@gmail.com"

                treesToSave =
                    selectList
                        |> List.filter .selected
                        |> List.map .tree
                        |> Import.encode author
            in
            ( ImportSaving selectList pageData, sendOut <| SaveImportedData treesToSave )

        ( Port ImportComplete, ImportSaving _ pageData ) ->
            ( Loading pageData, getDocumentList pageData.session )

        _ ->
            updatePageData msg (getData model)
                |> Tuple.mapFirst Loaded


updatePageData : Msg -> PageData -> ( PageData, Cmd Msg )
updatePageData msg model =
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
                    ( model, Task.perform (ImportFileLoaded username) (File.toString file) )

                Nothing ->
                    ( model, Cmd.none )

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

        _ ->
            ( model, Cmd.none )


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
    case model of
        Loading pageData ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , text "loading..."
                , button [ onClick GetNewDocId ] [ text "New" ]
                , button [ onClick ImportFileRequested ] [ text "Legacy Import" ]
                ]

        Loaded pageData ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , ul [] (List.map viewDocEntry pageData.documents)
                , button [ onClick GetNewDocId ] [ text "New" ]
                , button [ onClick ImportFileRequested ] [ text "Legacy Import" ]
                ]

        ImportSelecting selectionList pageData ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , ul [] (List.map viewSelectionEntry selectionList)
                , button [ onClick ImportSelectionDone ] [ text "Import Trees..." ]
                ]

        ImportSaving selectionList pageData ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , text "Importing selected trees...."
                ]

        Error err pageData ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , h1 [] [ text err ]
                , ul [] (List.map viewDocEntry pageData.documents)
                , button [ onClick GetNewDocId ] [ text "New" ]
                , button [ onClick ImportFileRequested ] [ text "Legacy Import" ]
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


viewSelectionEntry : { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li [] [ input [ type_ "checkbox", checked selected, onCheck (ImportTreeSelected id) ] [], text (Metadata.getDocName mdata |> Maybe.withDefault "Untitled") ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveMsg Port LogErr
