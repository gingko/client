port module Home exposing (..)

import Coders exposing (maybeToValue)
import Date exposing (Month(..))
import Date.Distance as DateDist
import Date.Extra as DateExtra
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode exposing (succeed)
import Json.Encode as Json exposing (null, string)
import Octicons as Icon
import Time exposing (Time, every, minute)


main : Program ( Time, List ( String, Document ) ) Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { documents : Dict String Document
    , archiveDropdown : Bool
    , currentTime : Time
    }


type alias Document =
    { name : Maybe String
    , state : String
    , created_at : String
    , last_modified : String
    }


defaultDocument : Document
defaultDocument =
    { name = Just "Untitled"
    , state = "active"
    , created_at = ""
    , last_modified = ""
    }


init : ( Time, List ( String, Document ) ) -> ( Model, Cmd Msg )
init ( time, dbObj ) =
    { documents = dbObj |> Dict.fromList
    , archiveDropdown = False
    , currentTime = time
    }
        ! []



-- UPDATE


type Msg
    = NoOp
    | New
    | Import
    | Open String (Maybe String)
    | OpenOther
    | SetState String String
    | Delete String
    | ToggleArchive
    | Tick Time
    | DocListReload (List ( String, Document ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            model ! [ forJS { tag = "New", data = null } ]

        Import ->
            model ! [ forJS { tag = "ImportGko", data = null } ]

        Open dbname docName_ ->
            let
                data =
                    [ string dbname, maybeToValue string docName_ ]
                        |> Json.list
            in
            model ! [ forJS { tag = "Open", data = data } ]

        OpenOther ->
            model ! [ forJS { tag = "OpenOther", data = null } ]

        SetState dbname state ->
            let
                data =
                    Json.list [ string dbname, string state ]
            in
            { model
                | documents =
                    model.documents
                        |> Dict.update dbname (Maybe.map (\v -> { v | state = state }))
            }
                ! [ forJS { tag = "SetState", data = data } ]

        Delete dbname ->
            { model
                | documents =
                    model.documents
                        |> Dict.filter (\k _ -> k /= dbname)
            }
                ! [ forJS { tag = "Delete", data = string dbname } ]

        ToggleArchive ->
            if (model.documents |> Dict.filter (\_ v -> v.state == "archived") |> Dict.size) /= 0 then
                ( { model | archiveDropdown = not model.archiveDropdown }, Cmd.none )
            else
                model ! []

        Tick currTime ->
            { model | currentTime = currTime } ! []

        DocListReload docList ->
            { model | documents = docList |> Dict.fromList } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html Msg
view { documents, archiveDropdown, currentTime } =
    let
        visibleWhen bool =
            classList [ ( "visible", bool ), ( "hidden", not bool ) ]

        numActive =
            documents
                |> Dict.filter (\_ v -> v.state == "active")
                |> Dict.size

        numArchived =
            documents
                |> Dict.filter (\_ v -> v.state == "archived")
                |> Dict.size

        archivedText bool =
            "Archived ("
                ++ (numArchived |> toString)
                ++ ")"
                ++ (case ( bool, numArchived == 0 ) of
                        ( _, True ) ->
                            ""

                        ( True, _ ) ->
                            " ▴"

                        ( False, _ ) ->
                            " ▾"
                   )
    in
    div [ id "container" ]
        [ div [ id "templates-block" ]
            [ div [ class "template-item", onClick New ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text "Blank" ]
                ]
            , div [ class "template-item", onClick Import ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "import", True ) ] ] [ Icon.file (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text "Import From File" ]
                , div [ class "template-description" ]
                    [ text "From Desktop or Online"
                    , br [] []
                    , text "(.gko or .json)"
                    ]
                ]
            ]
        , div [ id "documents-block" ]
            [ h4 [ class "list-section-header" ]
                [ text "Recent Documents"
                , span
                    [ class "list-header", visibleWhen (numActive /= 0) ]
                    [ div [] [ text "Last Modified" ]
                    ]
                ]
            , viewDocList currentTime "active" documents
            ]
        , div [ id "buttons-block" ]
            [ button [ onClick OpenOther ] [ text "Open Other Documents" ] ]
        ]


viewDocList : Time -> String -> Dict String Document -> Html Msg
viewDocList currTime state docDict =
    div [ classList [ ( "document-list", True ), ( state, True ) ] ]
        (docDict
            |> Dict.filter (\k v -> v.state == state)
            |> Dict.toList
            |> List.sortBy (\( k, v ) -> v.last_modified)
            |> List.reverse
            |> List.map (viewDocumentItem currTime)
        )


viewDocumentItem : Time -> ( String, Document ) -> Html Msg
viewDocumentItem currTime ( dbname, document ) =
    let
        onClickThis msg =
            onWithOptions "click" { defaultOptions | stopPropagation = True } (succeed msg)

        nowDate =
            Date.fromTime currTime

        modDate =
            document.last_modified
                |> DateExtra.fromIsoString
                |> Maybe.withDefault (DateExtra.fromCalendarDate 2000 Jan 1)

        modString =
            modDate |> DateExtra.toFormattedString "YYYY-MM-dd, HH:mm"

        relativeString =
            DateDist.inWords
                nowDate
                modDate
                ++ " ago"

        ( titleString, dateString ) =
            if DateExtra.diff DateExtra.Day modDate nowDate <= 2 then
                ( modString, relativeString )
            else
                ( relativeString, modString )

        buttons =
            case document.state of
                "archived" ->
                    [ div
                        [ onClickThis (Delete dbname), title "Delete document" ]
                        [ Icon.trashcan Icon.defaultOptions ]
                    , div
                        [ onClickThis (SetState dbname "active"), title "Restore document" ]
                        [ Icon.arrowUp Icon.defaultOptions ]
                    ]

                _ ->
                    [ div
                        [ onClickThis (SetState dbname "archived"), title "Remove From List" ]
                        [ Icon.x Icon.defaultOptions ]
                    ]
    in
    div
        [ class "document-item", onClick (Open dbname document.name) ]
        [ div [ class "doc-title" ] [ text (document.name |> Maybe.withDefault "Untitled") ]
        , div [ class "doc-modified", title titleString ] [ text dateString ]
        , div [ class "doc-buttons" ] buttons
        ]



-- SUBSCRIPTIONS


port forJS : { tag : String, data : Json.Value } -> Cmd msg


port docListReload : (List ( String, Document ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ docListReload DocListReload
        , every (minute / 2) Tick
        ]
