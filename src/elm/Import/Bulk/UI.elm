module Import.Bulk.UI exposing (Model, Msg, update, view)

import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (checked, class, classList, disabled, height, href, id, src, style, target, type_, width)
import Html.Events exposing (on, onCheck, onClick)
import Import.Bulk
import Json.Decode as Dec
import Outgoing exposing (Msg(..), send)
import Task
import Time
import Translation exposing (Language)
import Types exposing (Tree)
import User exposing (User)



-- MODEL


type alias Model =
    { state : ImportModalState, user : User }


type ImportModalState
    = Closed
    | ModalOpen { loginState : LoginState, isFileDragging : Bool }
    | ImportSelecting ImportSelection
    | ImportSaving ImportSelection


type LoginState
    = Unknown
    | LoggedIn
    | LoggedOut


type alias ImportSelection =
    List
        { selected : Bool
        , tree : ( String, Metadata, Tree )
        }



-- UPDATE


type Msg
    = NoOp
    | ModalToggled Bool
    | LegacyLoginStateChanged Bool
    | Retry
    | FileRequested
    | FileDraggedOver Bool
    | FileSelected File
    | FileLoaded String String
    | TreeSelected String Bool
    | SelectionDone
    | Completed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ state, user } as model) =
    case ( msg, state ) of
        ( ModalToggled True, Closed ) ->
            ( { model | state = ModalOpen { loginState = Unknown, isFileDragging = False } }, Cmd.none )

        ( ModalToggled False, _ ) ->
            ( { model | state = Closed }, Cmd.none )

        ( LegacyLoginStateChanged isLoggedIn, _ ) ->
            let
                newState =
                    if isLoggedIn then
                        LoggedIn

                    else
                        LoggedOut
            in
            ( { model | state = ModalOpen { loginState = newState, isFileDragging = False } }, Cmd.none )

        ( Retry, ModalOpen modalData ) ->
            ( { model | state = ModalOpen { modalData | loginState = Unknown } }, Cmd.none )

        ( FileRequested, _ ) ->
            ( model, Select.file [ "text/*", "application/json" ] FileSelected )

        ( FileDraggedOver isDraggedOver, ModalOpen modalState ) ->
            ( { model | state = ModalOpen { modalState | isFileDragging = isDraggedOver } }, Cmd.none )

        ( FileSelected file, _ ) ->
            case User.name model.user of
                Just username ->
                    ( model, Task.perform (FileLoaded username) (File.toString file) )

                Nothing ->
                    ( model, Cmd.none )

        ( FileLoaded _ contents, ModalOpen _ ) ->
            case Dec.decodeString Import.Bulk.decoder contents of
                Ok dataList ->
                    let
                        listWithSelectState =
                            dataList
                                |> List.sortBy (\( _, mdata, _ ) -> Metadata.getUpdatedAt mdata |> Time.posixToMillis)
                                |> List.reverse
                                |> List.map (\t -> { selected = False, tree = t })
                    in
                    ( { model | state = ImportSelecting listWithSelectState }, Cmd.none )

                Err _ ->
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
            ( { model | state = ImportSelecting newList }, Cmd.none )

        ( SelectionDone, ImportSelecting selectList ) ->
            let
                author =
                    user |> User.name |> Maybe.withDefault "jane.doe@gmail.com"

                treesToSave =
                    selectList
                        |> List.filter .selected
                        |> List.map .tree
                        |> Import.Bulk.encode author
            in
            ( { model | state = ImportSaving selectList }, send <| SaveImportedData treesToSave )

        ( Completed, ImportSaving _ ) ->
            ( { model | state = Closed }, DocList.fetch user )

        _ ->
            ( model, Cmd.none )



-- VIEW


view : Language -> Model -> List (Html Msg)
view lang { state } =
    case state of
        Closed ->
            [ text "" ]

        ModalOpen { loginState, isFileDragging } ->
            case loginState of
                Unknown ->
                    [ h1 [] [ text "Import From Gingko v1" ]
                    , text "Checking to see if you're logged in or not:"
                    , br [] []
                    , iframe [ src "https://gingkoapp.com/loggedin", width 0, height 0 ] []
                    ]
                        |> modalWrapper

                LoggedIn ->
                    let
                        fileDropDecoder =
                            Dec.map
                                (\files ->
                                    case List.head files of
                                        Just file ->
                                            FileSelected file

                                        Nothing ->
                                            NoOp
                                )
                                (Dec.field "dataTransfer" (Dec.field "files" (Dec.list File.decoder)))
                    in
                    [ h1 [] [ text "Import From Gingko v1" ]
                    , p [] [ text "To transfer multiple trees from your old account to this new one, follow these steps." ]
                    , p []
                        [ text "1. Click here to download a backup of all your trees: "
                        , br [] []
                        , a [ href "https://gingkoapp.com/export/all" ] [ text "Download Full Backup" ]
                        ]
                    , p []
                        [ text "2. Drag the backup file here:"
                        , div
                            [ classList [ ( "file-drop-zone", True ), ( "dragged-over", isFileDragging ) ]
                            , on "dragenter" (Dec.succeed (FileDraggedOver True))
                            , on "dragleave" (Dec.succeed (FileDraggedOver False))
                            , on "drop" fileDropDecoder
                            ]
                            []
                        , text "or find the file in your system: "
                        , button [ onClick FileRequested ] [ text "Browse..." ]
                        ]
                    ]
                        |> modalWrapper

                LoggedOut ->
                    [ h1 [] [ text "Import From Gingko v1" ]
                    , p [] [ text "To transfer trees from your old account, you need to be logged in to it." ]
                    , p [] [ text "But it seems you are not logged in to your old account." ]
                    , p []
                        [ text "1. "
                        , a [ href "https://gingkoapp.com/login", target "_blank" ] [ text "Login there" ]
                        , text "."
                        ]
                    , p []
                        [ text "2. Then, come back and ", button [ onClick Retry ] [ text "Try again" ], text "." ]
                    ]
                        |> modalWrapper

        ImportSelecting importSelection ->
            let
                isDisabled =
                    importSelection
                        |> List.any .selected
                        |> not
            in
            [ h1 [] [ text "Import From Gingko v1" ]
            , div [ style "display" "flex", style "margin-top" "10px" ] [ span [ style "flex" "auto" ] [ text "Name" ], span [] [ text "Last Modified" ] ]
            , div [ id "import-selection-list" ] [ ul [] (List.map (viewSelectionEntry lang) importSelection) ]
            , button [ onClick SelectionDone, disabled isDisabled ] [ text "Import Selected Trees" ]
            ]
                |> modalWrapper

        ImportSaving importSelection ->
            let
                importCount =
                    importSelection
                        |> List.filter .selected
                        |> List.length
            in
            [ h1 [] [ text "Import From Gingko v1" ]
            , p []
                [ text <| "Importing selected " ++ String.fromInt importCount ++ " trees..."
                , br [] []
                , text "This might take a while..."
                ]
            ]
                |> modalWrapper


modalWrapper : List (Html Msg) -> List (Html Msg)
modalWrapper body =
    [ div [ class "modal-overlay" ] []
    , div [ class "modal" ] [ button [ class "close-button", onClick (ModalToggled False) ] [ text "X" ], div [ class "modal-guts" ] body ]
    ]


viewSelectionEntry : Language -> { selected : Bool, tree : ( String, Metadata, Tree ) } -> Html Msg
viewSelectionEntry lang { selected, tree } =
    let
        ( id, mdata, _ ) =
            tree
    in
    li []
        [ span []
            [ input [ type_ "checkbox", checked selected, onCheck (TreeSelected id) ] []
            , text (Metadata.getDocName mdata |> Maybe.withDefault "Untitled")
            ]
        , span [] [ text (Metadata.getUpdatedAt mdata |> Translation.dateFormat lang) ]
        ]
