port module Doc.List exposing (Model(..), current, filter, getLastUpdated, init, isLoading, subscribe, switchListSort, toList, update, viewSidebarList, viewSwitcher)

import Ant.Icons.Svg as AntIcons
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, a, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, title, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave, stopPropagationOn)
import Json.Decode as Dec
import Page.Doc.ContextMenu as ContextMenu
import Route
import Time
import Translation exposing (TranslationId(..), timeDistInWords, tr)
import Types exposing (TooltipPosition(..))
import Utils exposing (onClickStop)



-- MODEL


type Model
    = Loading
    | Success (List Metadata)
    | Failure Dec.Error


type SortBy
    = Alphabetical
    | ModifiedAt
    | CreatedAt


init : Model
init =
    Loading


isLoading : Model -> Bool
isLoading model =
    if model == Loading then
        True

    else
        False


getLastUpdated : Model -> Maybe String
getLastUpdated model =
    case model of
        Success list ->
            list
                |> List.sortBy (Metadata.getUpdatedAt >> Time.posixToMillis)
                |> List.map Metadata.getDocId
                |> List.reverse
                |> List.head

        _ ->
            Nothing


filter : String -> Model -> Model
filter term model =
    case model of
        Success docList ->
            docList
                |> List.filter
                    (\m ->
                        m
                            |> Metadata.getDocName
                            |> Maybe.withDefault "Untitled"
                            |> String.toLower
                            |> String.contains (term |> String.toLower)
                    )
                |> Success

        _ ->
            model


sortBy : SortBy -> List Metadata -> List Metadata
sortBy criteria docList =
    case criteria of
        Alphabetical ->
            docList
                |> List.sortBy (Metadata.getDocName >> Maybe.withDefault "Untitled")

        ModifiedAt ->
            docList
                |> List.sortBy (Metadata.getUpdatedAt >> Time.posixToMillis)
                |> List.reverse

        CreatedAt ->
            docList
                |> List.sortBy (Metadata.getCreatedAt >> Time.posixToMillis)
                |> List.reverse


switchListSort : Metadata -> Model -> Model
switchListSort currentDoc model =
    case model of
        Success docList ->
            docList
                |> List.sortBy (Metadata.getUpdatedAt >> Time.posixToMillis)
                |> List.reverse
                |> List.filter (\d -> not (Metadata.isSameDocId d currentDoc))
                |> List.append [ currentDoc ]
                |> Success

        _ ->
            model


current : Metadata -> Model -> Maybe Metadata
current metadata model =
    case model of
        Success list ->
            list
                |> List.filter (\m -> Metadata.isSameDocId metadata m)
                |> List.head

        _ ->
            Nothing


toList : Model -> Maybe (List Metadata)
toList model =
    case model of
        Success docs ->
            Just docs

        _ ->
            Nothing



-- UPDATE


update : Model -> Model -> Model
update newModel oldModel =
    newModel



-- VIEW


viewSidebarList :
    { noOp : msg
    , filter : String -> msg
    , contextMenu : String -> ( Float, Float ) -> msg
    , tooltipRequested : String -> TooltipPosition -> String -> msg
    , tooltipClosed : msg
    }
    -> Metadata
    -> Maybe String
    -> String
    -> Model
    -> Html msg
viewSidebarList msgs currentDocument contextTarget_ filterField model =
    let
        sortCriteria =
            ModifiedAt

        stopClickProp =
            stopPropagationOn "click" (Dec.succeed ( msgs.noOp, True ))

        viewDocItem d =
            let
                docId =
                    Metadata.getDocId d
            in
            div
                [ classList
                    [ ( "sidebar-document-item", True )
                    , ( "active", Metadata.isSameDocId d currentDocument )
                    , ( "context-target", Just docId == contextTarget_ )
                    ]
                ]
                [ a
                    [ ContextMenu.open (msgs.contextMenu docId)
                    , href <| Route.toString (Route.DocUntitled docId)
                    , stopClickProp
                    , attribute "data-private" "lipsum"
                    ]
                    [ Metadata.getDocName d |> Maybe.withDefault "Untitled" |> text ]
                ]
    in
    case filter filterField model of
        Loading ->
            div [ id "sidebar-document-list-wrap" ]
                [ div [ id "sidebar-document-list" ] [ text "" ] ]

        Success filteredDocs ->
            if List.length filteredDocs == 0 then
                div [ id "sidebar-document-list-wrap" ]
                    [ div [ id "no-documents" ] [ text "No documents" ] ]

            else
                div [ id "sidebar-document-list-wrap" ]
                    [ div [ id "document-list-buttons" ]
                        [ div
                            [ id "sort-alphabetical"
                            , class "list-icon"
                            , onMouseEnter <| msgs.tooltipRequested "sort-alphabetical" BelowTooltip "Sort by Name"
                            , onMouseLeave msgs.tooltipClosed
                            ]
                            [ text "Abc" ]
                        , div
                            [ id "sort-modified"
                            , class "list-icon"
                            , class "selected"
                            , onMouseEnter <| msgs.tooltipRequested "sort-modified" BelowTooltip "Sort by Last Modified"
                            , onMouseLeave msgs.tooltipClosed
                            ]
                            [ AntIcons.editOutlined [] ]
                        , div
                            [ id "sort-created"
                            , class "list-icon"
                            , onMouseEnter <| msgs.tooltipRequested "sort-created" BelowTooltip "Sort by Date Created"
                            , onMouseLeave msgs.tooltipClosed
                            ]
                            [ AntIcons.fileOutlined [] ]
                        ]
                    , input [ id "document-list-filter", placeholder "Find file by name", type_ "search", onInput msgs.filter, stopClickProp ] []
                    , div [ id "sidebar-document-list" ] (List.map viewDocItem (sortBy sortCriteria filteredDocs))
                    ]

        Failure _ ->
            text "Failed to load documents list."


type alias SwitcherModel =
    { docList : Model
    , selected : String
    }


viewSwitcher : Metadata -> SwitcherModel -> Html msg
viewSwitcher currentDocument model =
    let
        viewDocItem d =
            div
                [ classList
                    [ ( "switcher-document-item", True )
                    , ( "current", Metadata.isSameDocId d currentDocument )
                    , ( "selected", Metadata.getDocId d == model.selected )
                    ]
                ]
                [ a [ href <| Route.toString (Route.DocUntitled (Metadata.getDocId d)), attribute "data-private" "lipsum" ]
                    [ Metadata.getDocName d |> Maybe.withDefault "Untitled" |> text ]
                ]
    in
    case model.docList of
        Loading ->
            text "Loading..."

        Success docs ->
            div [ class "switcher-document-list" ] (List.map viewDocItem docs)

        Failure _ ->
            text "Failed to load documents list."



-- DECODERS


decoderLocal : Dec.Value -> Model
decoderLocal json =
    case Dec.decodeValue Metadata.listDecoder json of
        Ok list ->
            Success list

        Err err ->
            Failure err



-- SUBSCRIPTIONS


port documentListChanged : (Dec.Value -> msg) -> Sub msg


subscribe : (Model -> msg) -> Sub msg
subscribe msg =
    documentListChanged (decoderLocal >> msg)
