port module Doc.List exposing (Model, current, fetch, filter, getLastUpdated, init, isLoading, sortByUpdated, subscribe, viewLarge, viewSmall, viewSwitcher)

import Date
import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, a, div, h1, li, text, ul)
import Html.Attributes exposing (class, classList, href, title)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Dec
import Octicons as Icon
import Outgoing exposing (Msg(..), send)
import Route
import Strftime
import Time
import Translation exposing (TranslationId(..), timeDistInWords, tr)
import User exposing (User)



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


fetch : User -> Cmd msg
fetch session =
    case User.db session of
        Just userDb ->
            send <| GetDocumentList userDb

        Nothing ->
            Cmd.none


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


sortByUpdated : Model -> Model
sortByUpdated model =
    case model of
        Success docList ->
            docList
                |> List.sortBy (Metadata.getUpdatedAt >> Time.posixToMillis)
                |> List.reverse
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



-- VIEW


type alias ListMsgs msg =
    { openDoc : String -> msg
    , deleteDoc : String -> msg
    }


viewLarge : ListMsgs msg -> Translation.Language -> Time.Posix -> Model -> Html msg
viewLarge msgs lang currTime model =
    case model of
        Loading ->
            h1 [] [ text "LOADING" ]

        Success docList ->
            viewDocListLoaded msgs lang currTime docList

        Failure _ ->
            text <| "error!"


viewDocListLoaded : ListMsgs msg -> Translation.Language -> Time.Posix -> List Metadata -> Html msg
viewDocListLoaded msgs lang currTime docList =
    div [ classList [ ( "document-list", True ) ] ]
        (docList
            |> List.sortBy (Time.posixToMillis << Metadata.getUpdatedAt)
            |> List.reverse
            |> List.map (viewDocumentItem msgs lang currTime)
        )


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


viewSmall : Metadata -> Model -> Html msg
viewSmall currentDocument model =
    let
        viewDocItem d =
            li [ classList [ ( "sidebar-document-item", True ), ( "active", Metadata.isSameDocId d currentDocument ) ] ]
                [ a [ href <| Route.toString (Route.DocUntitled (Metadata.getDocId d)) ]
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


viewSwitcher : Metadata -> Model -> Html msg
viewSwitcher currentDocument model =
    let
        viewDocItem d =
            div [ classList [ ( "switcher-document-item", True ), ( "current", Metadata.isSameDocId d currentDocument ) ] ]
                [ a [ href <| Route.toString (Route.DocUntitled (Metadata.getDocId d)) ]
                    [ Metadata.getDocName d |> Maybe.withDefault "Untitled" |> text ]
                ]
    in
    case model of
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
