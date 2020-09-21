port module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Html exposing (Html, a, button, div, h1, h4, input, li, span, text, ul)
import Html.Attributes exposing (checked, class, classList, href, id, type_)
import Html.Events exposing (onCheck, onClick)
import Import
import Json.Decode as Dec
import Octicons as Icon
import Outgoing exposing (Msg(..), send)
import Route
import Session exposing (Session)
import Task
import Time
import Translation exposing (..)
import Types exposing (Tree)



-- MODEL


type Model
    = Home PageData
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


type alias PageData =
    { documents : DocList.Model
    , language : Translation.Language
    , languageMenu : Bool
    , currentTime : Time.Posix
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( Home
        { documents = DocList.init
        , language = langFromString "en"
        , languageMenu = False
        , currentTime = Time.millisToPosix 0
        , session = session
        }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , DocList.fetch session ReceivedDocuments
        ]
    )


toSession : Model -> Session
toSession model =
    (getData model).session


getData : Model -> PageData
getData model =
    case model of
        Home pdata ->
            pdata

        ImportSelecting _ pdata ->
            pdata

        ImportSaving _ pdata ->
            pdata



-- UPDATE


type Msg
    = ReceivedDocuments DocList.Model
    | Open String
    | DeleteDoc String
    | ImportFileRequested
    | ImportFileSelected File
    | ImportTreeSelected String Bool
    | ImportFileLoaded String String
    | ImportSelectionDone
    | ImportComplete
    | ToggleLanguageMenu
    | ChangeLanguage Language
    | Tick Time.Posix
    | LogErr String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ImportFileLoaded _ contents, Home pageData ) ->
            case Dec.decodeString Import.decoder contents of
                Ok dataList ->
                    let
                        listWithSelectState =
                            dataList |> List.map (\t -> { selected = False, tree = t })
                    in
                    ( ImportSelecting listWithSelectState pageData, Cmd.none )

                Err _ ->
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
            ( ImportSaving selectList pageData, send <| SaveImportedData treesToSave )

        ( ImportComplete, ImportSaving _ pageData ) ->
            ( Home pageData, DocList.fetch pageData.session ReceivedDocuments )

        ( Tick _, ImportSelecting _ _ ) ->
            ( model, Cmd.none )

        _ ->
            updatePageData msg (getData model)
                |> Tuple.mapFirst Home


updatePageData : Msg -> PageData -> ( PageData, Cmd Msg )
updatePageData msg model =
    case msg of
        ReceivedDocuments response ->
            ( { model | documents = DocList.update response model.documents }, Cmd.none )

        Open docId ->
            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )

        DeleteDoc docId ->
            ( model, send <| RequestDelete docId )

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

        LogErr err ->
            ( model
            , send (ConsoleLogRequested err)
            )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Home pageData ->
            viewHome pageData

        ImportSelecting selectionList _ ->
            div [ id "import-selecting" ]
                [ h1 [] [ text "This is the home page" ]
                , ul [] (List.map viewSelectionEntry selectionList)
                , button [ onClick ImportSelectionDone ] [ text "Import Trees..." ]
                ]

        ImportSaving _ _ ->
            div []
                [ h1 [] [ text "This is the home page" ]
                , text "Importing selected trees...."
                ]


viewHome : PageData -> Html Msg
viewHome { language, languageMenu, currentTime, documents } =
    div [ id "container" ]
        [ div [ id "templates-block" ]
            [ a [ id "template-new", class "template-item", href (Route.toString Route.DocNew) ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text <| tr language HomeBlank ]
                ]
            , div [ id "template-import", class "template-item", onClick ImportFileRequested ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "import", True ) ] ] [ Icon.file (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text <| tr language HomeImportLegacy ]
                , div [ class "template-description" ]
                    [ text <| tr language HomeLegacyFrom ]
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
            , DocList.viewLarge { openDoc = Open, deleteDoc = DeleteDoc } language currentTime documents
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


viewSelectionEntry : { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li [] [ input [ type_ "checkbox", checked selected, onCheck (ImportTreeSelected id) ] [], text (Metadata.getDocName mdata |> Maybe.withDefault "Untitled") ]



-- SUBSCRIPTIONS


port importComplete : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ importComplete (always ImportComplete)
        , DocList.subscribe ReceivedDocuments
        , Time.every (30 * 1000) Tick
        ]
