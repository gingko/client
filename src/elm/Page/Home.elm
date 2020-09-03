module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Date
import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, button, div, h1, h4, input, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, href, id, title, type_)
import Html.Events exposing (onCheck, onClick, stopPropagationOn)
import Http
import Import
import Json.Decode as Dec
import Octicons as Icon
import Ports exposing (IncomingMsg(..), OutgoingMsg(..), receiveMsg, sendOut)
import RandomId
import Route
import Session exposing (Session)
import Strftime
import Task
import Time
import Translation exposing (..)
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
    , languageMenu : Bool
    , currentTime : Time.Posix
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Loading
        { documents = []
        , language = langFromString "en"
        , languageMenu = False
        , currentTime = Time.millisToPosix 0
        , session = session
        }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , getDocumentList session
        ]
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
    | Open String
    | DeleteDoc String
    | ImportFileRequested
    | ImportFileSelected File
    | ImportTreeSelected String Bool
    | ImportFileLoaded String String
    | ImportSelectionDone
    | ToggleLanguageMenu
    | ChangeLanguage Language
    | Tick Time.Posix
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

        Open docId ->
            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

        DeleteDoc docId ->
            ( model, sendOut <| RequestDelete docId )

        ToggleLanguageMenu ->
            ( { model | languageMenu = not model.languageMenu }, Cmd.none )

        ChangeLanguage lang ->
            ( { model | language = lang }, Cmd.none )

        ImportFileRequested ->
            ( model, Select.file [ "text/*", "application/json" ] ImportFileSelected )

        ImportFileSelected file ->
            case Session.username model.session of
                Just username ->
                    ( model, Task.perform (ImportFileLoaded username) (File.toString file) )

                Nothing ->
                    ( model, Cmd.none )

        Tick currTime ->
            ( { model | currentTime = currTime }, Cmd.none )

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
                ]

        Loading pageData ->
            viewHome pageData

        Loaded pageData ->
            viewHome pageData


viewHome : PageData -> Html Msg
viewHome { language, languageMenu, currentTime, documents } =
    let
        visibleWhen bool =
            classList [ ( "visible", bool ), ( "hidden", not bool ) ]
    in
    div [ id "container" ]
        [ div [ id "templates-block" ]
            [ div [ id "template-new", class "template-item", onClick GetNewDocId ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text <| tr language HomeBlank ]
                ]
            , div [ id "template-import", class "template-item", onClick ImportFileRequested ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "import", True ) ] ] [ Icon.file (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text <| tr language HomeImportJSON ]
                , div [ class "template-description" ]
                    [ text <| tr language HomeJSONFrom ]
                ]
            ]
        , div [ id "documents-block" ]
            [ h4 [ class "list-section-header" ]
                [ text "."
                , span
                    [ class "list-header" ]
                    [ div [] [ text <| tr language LastUpdated ]
                    ]
                ]
            , viewDocList language currentTime documents
            ]
        , div [ id "buttons-block" ]
            [ div [ onClick ToggleLanguageMenu, classList [ ( "document-item", True ), ( "language-button", True ) ] ]
                (if languageMenu then
                    Translation.activeLanguages
                        |> List.map
                            (\( l, n ) ->
                                div
                                    [ classList [ ( "language-item", True ), ( "selected", l == language ) ]
                                    , onClick <| ChangeLanguage l
                                    ]
                                    [ text <| n ]
                            )

                 else
                    [ div [ class "language-item" ] [ text <| Translation.languageName language ] ]
                )
            ]
        ]


viewDocList : Translation.Language -> Time.Posix -> List Metadata -> Html Msg
viewDocList lang currTime docList =
    div [ classList [ ( "document-list", True ) ] ]
        (docList
            |> List.sortBy (Time.posixToMillis << Metadata.getUpdatedAt)
            |> List.reverse
            |> List.map (viewDocumentItem lang currTime)
        )


viewDocumentItem : Translation.Language -> Time.Posix -> Metadata -> Html Msg
viewDocumentItem lang currTime metadata =
    let
        docId =
            Metadata.getDocId metadata

        docName_ =
            Metadata.getDocName metadata

        onClickThis msg =
            stopPropagationOn "click" (Dec.succeed ( msg, True ))

        -- TODO: fix timezone
        currDate =
            Date.fromPosix Time.utc currTime

        updatedTime =
            Metadata.getUpdatedAt metadata

        -- TODO: fix timezone
        updatedDate =
            Date.fromPosix Time.utc updatedTime

        -- TODO: fix timezone
        updatedString =
            updatedTime
                |> Strftime.format "%Y-%m-%d, %H:%M" Time.utc

        relativeString =
            timeDistInWords
                lang
                updatedTime
                currTime

        ( titleString, dateString ) =
            if Date.diff Date.Days updatedDate currDate <= 2 then
                ( updatedString, relativeString )

            else
                ( relativeString, updatedString )

        buttons =
            [ div
                [ onClickThis (DeleteDoc docId), title <| tr lang RemoveFromList ]
                [ Icon.x Icon.defaultOptions ]
            ]
    in
    div
        [ class "document-item", onClick (Open docId) ]
        [ div [ class "doc-title" ] [ text (docName_ |> Maybe.withDefault "Untitled") ]
        , div [ class "doc-opened", title titleString ] [ text dateString ]
        , div [ class "doc-buttons" ] buttons
        ]


viewSelectionEntry : { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li [] [ input [ type_ "checkbox", checked selected, onCheck (ImportTreeSelected id) ] [], text (Metadata.getDocName mdata |> Maybe.withDefault "Untitled") ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveMsg Port LogErr
        , Time.every (30 * 1000) Tick
        ]
