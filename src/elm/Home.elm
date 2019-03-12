port module Home exposing (Document, Model, Msg(..), docListReload, forJS, init, main, subscriptions, update, view, viewDocList, viewDocumentItem)

import Browser
import Coders exposing (maybeToValue)
import Date
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Iso8601
import Json.Decode exposing (succeed)
import Json.Encode as Json exposing (null, string)
import Octicons as Icon
import Strftime
import Time
import Time.Distance as TimeDistance
import Translation exposing (TranslationId(..), langFromString, tr)


main : Program ( Int, List ( String, Document ), String ) Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { documents : Dict String Document
    , archiveDropdown : Bool
    , currentTime : Time.Posix
    , language : Translation.Language
    }


type alias Document =
    { name : Maybe String
    , state : String
    , created_at : String
    , last_modified : String
    , last_opened : String
    }


init : ( Int, List ( String, Document ), String ) -> ( Model, Cmd Msg )
init ( time, dbObj, langString ) =
    ( { documents = dbObj |> Dict.fromList
      , archiveDropdown = False
      , currentTime = Time.millisToPosix time
      , language = langFromString langString
      }
    , Cmd.none
    )



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
    | Tick Time.Posix
    | DocListReload (List ( String, Document ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        New ->
            ( model
            , forJS { tag = "New", data = null }
            )

        Import ->
            ( model
            , forJS { tag = "ImportGko", data = null }
            )

        Open dbname docName_ ->
            let
                data =
                    [ string dbname, maybeToValue string docName_ ]
                        |> Json.list identity
            in
            ( model
            , forJS { tag = "Open", data = data }
            )

        OpenOther ->
            ( model
            , forJS { tag = "OpenOther", data = null }
            )

        SetState dbname state ->
            let
                data =
                    Json.list string [ dbname, state ]
            in
            ( { model
                | documents =
                    model.documents
                        |> Dict.update dbname (Maybe.map (\v -> { v | state = state }))
              }
            , forJS { tag = "SetState", data = data }
            )

        Delete dbname ->
            ( { model
                | documents =
                    model.documents
                        |> Dict.filter (\k _ -> k /= dbname)
              }
            , forJS { tag = "Delete", data = string dbname }
            )

        ToggleArchive ->
            if (model.documents |> Dict.filter (\_ v -> v.state == "archived") |> Dict.size) /= 0 then
                ( { model | archiveDropdown = not model.archiveDropdown }, Cmd.none )

            else
                ( model
                , Cmd.none
                )

        Tick currTime ->
            ( { model | currentTime = currTime }
            , Cmd.none
            )

        DocListReload docList ->
            ( { model | documents = docList |> Dict.fromList }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view { documents, archiveDropdown, currentTime, language } =
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
    in
    div [ id "container" ]
        [ div [ id "templates-block" ]
            [ div [ class "template-item", onClick New ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                , div [ class "template-title" ] [ text <| tr language HomeBlank ]
                ]
            , div [ class "template-item", onClick Import ]
                [ div [ classList [ ( "template-thumbnail", True ), ( "import", True ) ] ] [ Icon.file (Icon.defaultOptions |> Icon.size 48) ]
                , div [ class "template-title" ] [ text <| tr language HomeImportJSON ]
                , div [ class "template-description" ]
                    [ text <| tr language HomeJSONFrom ]
                ]
            ]
        , div [ id "documents-block" ]
            [ h4 [ class "list-section-header" ]
                [ text <| tr language RecentDocuments
                , span
                    [ class "list-header", visibleWhen (numActive /= 0) ]
                    [ div [] [ text <| tr language LastOpened ]
                    ]
                ]
            , viewDocList language currentTime "active" documents
            ]
        , div [ id "buttons-block" ]
            [ div [ onClick OpenOther, class "document-item" ]
                [ text <| tr language OpenOtherDocuments ]
            ]
        ]


viewDocList : Translation.Language -> Time.Posix -> String -> Dict String Document -> Html Msg
viewDocList lang currTime state docDict =
    div [ classList [ ( "document-list", True ), ( state, True ) ] ]
        (docDict
            |> Dict.filter (\k v -> v.state == state)
            |> Dict.toList
            |> List.sortBy (\( k, v ) -> v.last_opened)
            |> List.reverse
            |> List.map (viewDocumentItem lang currTime)
        )


viewDocumentItem : Translation.Language -> Time.Posix -> ( String, Document ) -> Html Msg
viewDocumentItem lang currTime ( dbname, document ) =
    let
        onClickThis msg =
            stopPropagationOn "click" (succeed ( msg, True ))

        -- TODO: fix timezone
        currDate =
            Date.fromPosix Time.utc currTime

        openedTime =
            Iso8601.toTime document.last_opened
                |> Result.withDefault currTime

        -- TODO: fix timezone
        openedDate =
            Date.fromPosix Time.utc openedTime

        -- TODO: fix timezone
        openedString =
            openedTime
                |> Strftime.format "%Y-%m-%d, %H:%M" Time.utc

        relativeString =
            TimeDistance.inWords currTime openedTime
                ++ " ago"

        ( titleString, dateString ) =
            if Date.diff Date.Days openedDate currDate <= 2 then
                ( openedString, relativeString )

            else
                ( relativeString, openedString )

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
                        [ onClickThis (SetState dbname "archived"), title <| tr lang RemoveFromList ]
                        [ Icon.x Icon.defaultOptions ]
                    ]
    in
    div
        [ class "document-item", onClick (Open dbname document.name) ]
        [ div [ class "doc-title" ] [ text (document.name |> Maybe.withDefault "Untitled") ]
        , div [ class "doc-opened", title titleString ] [ text dateString ]
        , div [ class "doc-buttons" ] buttons
        ]



-- SUBSCRIPTIONS


port forJS : { tag : String, data : Json.Value } -> Cmd msg


port docListReload : (List ( String, Document ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ docListReload DocListReload
        , Time.every (30 * 1000) Tick
        ]
