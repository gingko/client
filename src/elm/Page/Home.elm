port module Page.Home exposing (Model, Msg, init, subscriptions, toUser, update, view)

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
import Task
import Time
import Translation exposing (..)
import Types exposing (Tree)
import User exposing (User)



-- MODEL


type alias Model =
    { documents : DocList.Model
    , importModal : ImportModalState
    , languageMenu : Bool
    , currentTime : Time.Posix
    , user : User
    }


type ImportModalState
    = Closed
    | ModalOpen
    | ImportSelecting ImportSelection
    | ImportSaving ImportSelection


type alias ImportSelection =
    List
        { selected : Bool
        , tree : ( String, Metadata, Tree )
        }


init : User -> ( Model, Cmd Msg )
init user =
    ( { documents = DocList.init
      , languageMenu = False
      , importModal = Closed
      , currentTime = Time.millisToPosix 0
      , user = user
      }
    , Cmd.batch
        [ Task.perform Tick Time.now
        , DocList.fetch user
        ]
    )


toUser : Model -> User
toUser =
    .user



-- UPDATE


type Msg
    = ReceivedDocuments DocList.Model
    | Open String
    | DeleteDoc String
    | ImportModal ImportModalMsg
    | ToggleLanguageMenu
    | ChangeLanguage Language
    | SettingsChanged Language
    | Tick Time.Posix
    | LogErr String


type ImportModalMsg
    = ModalToggled Bool
    | FileRequested
    | FileSelected File
    | FileLoaded String String
    | TreeSelected String Bool
    | SelectionDone
    | Completed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDocuments newList ->
            ( { model | documents = newList }, Cmd.none )

        Open docId ->
            ( model, Route.pushUrl (User.navKey model.user) (Route.DocUntitled docId) )

        DeleteDoc docId ->
            ( model, send <| RequestDelete docId )

        ToggleLanguageMenu ->
            ( { model | languageMenu = not model.languageMenu }, Cmd.none )

        ChangeLanguage lang ->
            ( { model | user = User.setLanguage lang model.user }, send <| SetLanguage lang )

        SettingsChanged lang ->
            ( { model | user = User.setLanguage lang model.user }, Cmd.none )

        ImportModal importMsg ->
            updateImportModal importMsg model |> Tuple.mapSecond (Cmd.map ImportModal)

        Tick currTime ->
            ( { model | currentTime = currTime }, Cmd.none )

        LogErr err ->
            ( model
            , send (ConsoleLogRequested err)
            )


updateImportModal : ImportModalMsg -> Model -> ( Model, Cmd ImportModalMsg )
updateImportModal msg ({ importModal, user } as model) =
    case ( msg, importModal ) of
        ( ModalToggled True, Closed ) ->
            ( { model | importModal = ModalOpen }, Cmd.none )

        ( ModalToggled False, _ ) ->
            ( { model | importModal = Closed }, Cmd.none )

        ( FileRequested, _ ) ->
            ( model, Select.file [ "text/*", "application/json" ] FileSelected )

        ( FileSelected file, _ ) ->
            case User.name model.user of
                Just username ->
                    ( model, Task.perform (FileLoaded username) (File.toString file) )

                Nothing ->
                    ( model, Cmd.none )

        ( FileLoaded _ contents, ModalOpen ) ->
            case Dec.decodeString Import.decoder contents of
                Ok dataList ->
                    let
                        listWithSelectState =
                            dataList |> List.map (\t -> { selected = False, tree = t })
                    in
                    ( { model | importModal = ImportSelecting listWithSelectState }, Cmd.none )

                Err err ->
                    let
                        _ =
                            Debug.log "Import File Error" err
                    in
                    ( model, Cmd.none )

        ( TreeSelected treeId isSelected, ImportSelecting selectList ) ->
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
            ( { model | importModal = ImportSelecting newList }, Cmd.none )

        ( SelectionDone, ImportSelecting selectList ) ->
            let
                author =
                    user |> User.name |> Maybe.withDefault "jane.doe@gmail.com"

                treesToSave =
                    selectList
                        |> List.filter .selected
                        |> List.map .tree
                        |> Import.encode author
            in
            ( { model | importModal = ImportSaving selectList }, send <| SaveImportedData treesToSave )

        ( Completed, ImportSaving _ ) ->
            ( { model | importModal = Closed }, DocList.fetch user )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { user, importModal, languageMenu, currentTime, documents } =
    let
        language =
            User.language user
    in
    div [ id "container" ]
        ([ div [ id "templates-block" ]
            [ a [ id "template-new", class "template-item", href (Route.toString Route.DocNew) ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text <| tr language HomeBlank ]
                ]
            , div [ id "template-import", class "template-item", onClick <| ImportModal (ModalToggled True) ]
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
            ++ viewImportModal importModal
        )


viewImportModal : ImportModalState -> List (Html Msg)
viewImportModal modalState =
    case modalState of
        Closed ->
            [ text "" ]

        ModalOpen ->
            [ h1 [] [ text "Import From Gingko v1" ]
            , text "Instructions here"
            , button [ onClick (ImportModal FileRequested) ] [ text "From backup file..." ]
            ]
                |> modalWrapper

        ImportSelecting importSelection ->
            [ h1 [] [ text "Import From Gingko v1" ]
            , div [] [ ul [] (List.map viewSelectionEntry importSelection) ]
            , button [ onClick (ImportModal SelectionDone) ] [ text "Import Selected Trees" ]
            ]
                |> modalWrapper

        ImportSaving importSelection ->
            [ h1 [] [ text "Import From Gingko v1" ]
            , div [] [ ul [] (List.map viewSelectionEntry importSelection) ]
            ]
                |> modalWrapper


modalWrapper : List (Html Msg) -> List (Html Msg)
modalWrapper body =
    [ div [ class "modal-overlay" ] []
    , div [ class "modal" ] [ button [ class "close-button" ] [ text "X" ], div [ class "modal-guts" ] body ]
    ]


viewSelectionEntry : { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li [] [ input [ type_ "checkbox", checked selected, onCheck (ImportModal << TreeSelected id) ] [], text (Metadata.getDocName mdata |> Maybe.withDefault "Untitled") ]



-- SUBSCRIPTIONS


port importComplete : (() -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ importComplete (always (ImportModal Completed))
        , DocList.subscribe ReceivedDocuments
        , User.settingsChange SettingsChanged
        , Time.every (30 * 1000) Tick
        ]
