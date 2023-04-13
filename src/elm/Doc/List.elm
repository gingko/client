port module Doc.List exposing (Model(..), current, filter, fromList, getLastUpdated, init, isLoading, subscribe, switchListSort, toList, update, viewSidebarList, viewSwitcher)

import Ant.Icons.Svg as AntIcons
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, a, div, input, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, title, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave, stopPropagationOn)
import Json.Decode as Dec
import Page.Doc.ContextMenu as ContextMenu
import Route
import Svg.Attributes
import Time
import Translation exposing (TranslationId(..), timeDistInWords, tr)
import Types exposing (SortBy(..), TooltipPosition(..))
import Utils exposing (onClickStop)



-- MODEL


type Model
    = Loading
    | Success (List Metadata)
    | Failure Dec.Error


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
                |> List.sortBy (Metadata.getDocName >> Maybe.withDefault "Untitled" >> String.toLower)

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


fromList : List Metadata -> Model
fromList docs =
    Success docs



-- UPDATE


update : SortBy -> Model -> Model -> Model
update sortCriteria newModel oldModel =
    case newModel of
        Success docList ->
            sortBy sortCriteria docList |> Success

        _ ->
            newModel



-- VIEW


viewSidebarList :
    { noOp : msg
    , filter : String -> msg
    , changeSortBy : SortBy -> msg
    , contextMenu : String -> ( Float, Float ) -> msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }
    -> String
    -> SortBy
    -> Maybe String
    -> String
    -> Model
    -> Html msg
viewSidebarList msgs currentDocId sortCriteria contextTarget_ filterField model =
    let
        stopClickProp =
            stopPropagationOn "click" (Dec.succeed ( msgs.noOp, True ))

        viewDocItem d =
            let
                itemDocId =
                    Metadata.getDocId d
            in
            div
                [ classList
                    [ ( "sidebar-document-item", True )
                    , ( "active", itemDocId == currentDocId )
                    , ( "context-target", Just itemDocId == contextTarget_ )
                    ]
                ]
                [ a
                    [ ContextMenu.open (msgs.contextMenu itemDocId)
                    , href <| Route.toString (Route.DocUntitled itemDocId)
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
            div [ id "sidebar-document-list-wrap" ]
                [ div [ id "document-list-buttons", onClickStop msgs.noOp ]
                    [ div
                        [ id "sort-alphabetical"
                        , class "sort-button"
                        , classList [ ( "selected", sortCriteria == Alphabetical ) ]
                        , onClickStop <| msgs.changeSortBy Alphabetical
                        , onMouseEnter <| msgs.tooltipRequested "sort-alphabetical" AboveTooltip SortByName
                        , onMouseLeave msgs.tooltipClosed
                        ]
                        [ text "Abc" ]
                    , div
                        [ id "sort-modified"
                        , class "sort-button"
                        , classList [ ( "selected", sortCriteria == ModifiedAt ) ]
                        , onClickStop <| msgs.changeSortBy ModifiedAt
                        , onMouseEnter <| msgs.tooltipRequested "sort-modified" AboveTooltip SortByLastModified
                        , onMouseLeave msgs.tooltipClosed
                        ]
                        [ AntIcons.editOutlined [ Svg.Attributes.class "sort-icon" ] ]
                    , div
                        [ id "sort-created"
                        , class "sort-button"
                        , classList [ ( "selected", sortCriteria == CreatedAt ) ]
                        , onClickStop <| msgs.changeSortBy CreatedAt
                        , onMouseEnter <| msgs.tooltipRequested "sort-created" AboveTooltip SortByDateCreated
                        , onMouseLeave msgs.tooltipClosed
                        ]
                        [ AntIcons.fileOutlined [ Svg.Attributes.class "sort-icon" ] ]
                    ]
                , input [ id "document-list-filter", placeholder "Find file by name", type_ "search", onInput msgs.filter, stopClickProp ] []
                , div [ id "sidebar-document-list" ]
                    (if not <| List.isEmpty filteredDocs then
                        List.map viewDocItem (sortBy sortCriteria filteredDocs)

                     else
                        [ div [ id "no-documents" ] [ text "No Documents Found" ] ]
                    )
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
                    , ( "current", Metadata.getDocId d == Metadata.getDocId currentDocument )
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
