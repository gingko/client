port module Doc.List exposing (Model(..), current, filter, getLastUpdated, init, isLoading, subscribe, switchListSort, toList, update, viewSmall, viewSwitcher)

import Date
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, a, div, li, text, ul)
import Html.Attributes exposing (attribute, class, classList, href, title)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Dec
import Octicons as Icon
import Page.Doc.ContextMenu as ContextMenu
import Route
import Strftime
import Time
import Translation exposing (TranslationId(..), timeDistInWords, tr)



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


type alias ListMsgs msg =
    { openDoc : String -> msg
    , deleteDoc : String -> msg
    }


viewDocumentItem : ListMsgs msg -> Translation.Language -> Time.Posix -> Metadata -> Html msg
viewDocumentItem msgs lang currTime metadata =
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
                [ onClickThis (msgs.deleteDoc docId), title <| tr lang DeleteDocument ]
                [ Icon.x Icon.defaultOptions ]
            ]
    in
    div
        [ class "document-item", onClick <| msgs.openDoc docId ]
        [ a [ href <| "/" ++ docId, class "doc-title" ] [ text (docName_ |> Maybe.withDefault "Untitled") ]
        , div [ class "doc-opened", title titleString ] [ text dateString ]
        , div [ class "doc-buttons" ] buttons
        ]


viewSmall : (String -> ( Float, Float ) -> msg) -> Metadata -> Model -> Html msg
viewSmall msg currentDocument model =
    let
        viewDocItem d =
            let
                docId =
                    Metadata.getDocId d
            in
            li [ classList [ ( "sidebar-document-item", True ), ( "active", Metadata.isSameDocId d currentDocument ) ] ]
                [ a
                    [ ContextMenu.open (msg docId)
                    , href <| Route.toString (Route.DocUntitled docId)
                    , attribute "data-private" "lipsum"
                    ]
                    [ Metadata.getDocName d |> Maybe.withDefault "Untitled" |> text ]
                ]
    in
    case model of
        Loading ->
            text "Loading..."

        Success docs ->
            ul [ class "sidebar-document-list" ] (List.map viewDocItem docs)

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
