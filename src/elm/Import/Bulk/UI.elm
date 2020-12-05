module Import.Bulk.UI exposing (Model, Msg, update)

import Doc.List as DocList
import Doc.Metadata as Metadata exposing (Metadata)
import File exposing (File)
import File.Select as Select
import Import.Bulk
import Json.Decode as Dec
import Outgoing exposing (Msg(..), send)
import Task
import Time
import Types exposing (Tree)
import User exposing (User)



-- MODEL


type alias Model =
    { importModal : ImportModalState, user : User }


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
    = ModalToggled Bool
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
update msg ({ importModal, user } as model) =
    case ( msg, importModal ) of
        ( ModalToggled True, Closed ) ->
            ( { model | importModal = ModalOpen { loginState = Unknown, isFileDragging = False } }, Cmd.none )

        ( ModalToggled False, _ ) ->
            ( { model | importModal = Closed }, Cmd.none )

        ( LegacyLoginStateChanged isLoggedIn, _ ) ->
            let
                newState =
                    if isLoggedIn then
                        LoggedIn

                    else
                        LoggedOut
            in
            ( { model | importModal = ModalOpen { loginState = newState, isFileDragging = False } }, Cmd.none )

        ( Retry, ModalOpen modalData ) ->
            ( { model | importModal = ModalOpen { modalData | loginState = Unknown } }, Cmd.none )

        ( FileRequested, _ ) ->
            ( model, Select.file [ "text/*", "application/json" ] FileSelected )

        ( FileDraggedOver isDraggedOver, ModalOpen modalState ) ->
            ( { model | importModal = ModalOpen { modalState | isFileDragging = isDraggedOver } }, Cmd.none )

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
                    ( { model | importModal = ImportSelecting listWithSelectState }, Cmd.none )

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
            ( { model | importModal = ImportSelecting newList }, Cmd.none )

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
            ( { model | importModal = ImportSaving selectList }, send <| SaveImportedData treesToSave )

        ( Completed, ImportSaving _ ) ->
            ( { model | importModal = Closed }, DocList.fetch user )

        _ ->
            ( model, Cmd.none )
