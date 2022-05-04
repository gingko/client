port module Home exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Html exposing (Attribute, Html, a, button, div, h1, h2, li, ul)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick, stopPropagationOn)
import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc
import Octicons as Icon exposing (defaultOptions)
import Time
import Translation exposing (Language(..), TranslationId(..), dateFormat, datetimeFormat, langToString, languageDecoder, timeDistInWords, tr)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Gingko Writer - Home" (view m)
        , subscriptions = always <| Time.every 2000 TimeUpdate
        }



-- MODEL


type alias Model =
    { recentDocuments : List RecentDocument
    , language : Language
    , currentTime : Time.Posix
    }


type alias RecentDocument =
    { name : String
    , path : String
    , birthtimeMs : Time.Posix
    , atimeMs : Time.Posix
    , mtimeMs : Time.Posix
    }


modelCodec : Codec Model
modelCodec =
    Codec.object Model
        |> Codec.field "recentDocuments" .recentDocuments (Codec.list recentDocumentCodec)
        |> Codec.field "language" .language (Codec.build (langToString >> Enc.string) languageDecoder)
        |> Codec.field "currentTime" .currentTime (Codec.float |> Codec.map (round >> Time.millisToPosix) (toFloat << Time.posixToMillis))
        |> Codec.buildObject


recentDocumentCodec : Codec RecentDocument
recentDocumentCodec =
    Codec.object RecentDocument
        |> Codec.field "name" .name Codec.string
        |> Codec.field "path" .path Codec.string
        |> Codec.field "birthtimeMs" .birthtimeMs (Codec.float |> Codec.map (round >> Time.millisToPosix) (toFloat << Time.posixToMillis))
        |> Codec.field "atimeMs" .atimeMs (Codec.float |> Codec.map (round >> Time.millisToPosix) (toFloat << Time.posixToMillis))
        |> Codec.field "mtimeMs" .mtimeMs (Codec.float |> Codec.map (round >> Time.millisToPosix) (toFloat << Time.posixToMillis))
        |> Codec.buildObject


init : Value -> ( Model, Cmd Msg )
init json =
    case Codec.decodeValue modelCodec json of
        Ok model ->
            ( model, Cmd.none )

        Err err ->
            ( Model [] En (Time.millisToPosix 0), Cmd.none )



-- UPDATE


type Msg
    = ClickedNew
    | ClickedOpen
    | ClickedImport
    | ClickedDocument String
    | ClickedRemoveDocument String
    | TimeUpdate Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedNew ->
            ( model, send ( "ClickedNew", Enc.null ) )

        ClickedOpen ->
            ( model, send ( "ClickedOpen", Enc.null ) )

        ClickedImport ->
            ( model, send ( "ClickedImport", Enc.null ) )

        ClickedDocument docPath ->
            ( model, send ( "ClickedDocument", Enc.string docPath ) )

        ClickedRemoveDocument docPath ->
            ( { model | recentDocuments = model.recentDocuments |> List.filter (\rd -> rd.path /= docPath) }, send ( "ClickedRemoveDocument", Enc.string docPath ) )

        TimeUpdate newTime ->
            ( { model | currentTime = newTime }, Cmd.none )



-- VIEW


view : Model -> List (Html Msg)
view model =
    let
        docItems =
            model.recentDocuments
                |> List.sortBy (Time.posixToMillis << .atimeMs)
                |> List.reverse
                |> List.map (viewDocItem model.language model.currentTime)
    in
    [ div [ id "container" ]
        [ div [ id "new-row" ]
            [ div [ class "template-row" ]
                [ div [ id "template-new", class "template-item", onClick ClickedNew ]
                    [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                    , div [ class "template-title" ] [ text model.language HomeBlank ]
                    ]
                ]
            ]
        , div [ id "open-import-row" ]
            [ div [ class "template-row" ]
                [ div [ id "template-open", class "template-item", onClick ClickedOpen ]
                    [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileDirectory (defaultOptions |> Icon.size 48) ]
                    , div [ class "template-title" ] [ textNoTr "Open" ]
                    ]
                , div [ id "template-import", class "template-item", onClick ClickedImport ]
                    [ div [ classList [ ( "template-thumbnail", True ) ] ] [ Icon.fileCode (defaultOptions |> Icon.size 48) ]
                    , div [ class "template-title" ] [ text model.language HomeImportJSON ]
                    ]
                ]
            ]
        , div [ id "recent-documents-block" ]
            ([ div [ class "list-section-header" ]
                [ text model.language RecentDocuments
                , div [ class "list-header" ] [ text model.language LastOpened ]
                ]
             ]
                ++ docItems
            )
        ]
    ]


viewDocItem : Language -> Time.Posix -> RecentDocument -> Html Msg
viewDocItem lang currentTime docItem =
    let
        ( lastOpenedString, lastOpenedTitle ) =
            if (Time.posixToMillis currentTime - Time.posixToMillis docItem.atimeMs) < 1000 * 3600 * 24 * 1 then
                ( timeDistInWords lang docItem.atimeMs currentTime, datetimeFormat lang docItem.atimeMs )

            else
                ( timeDistInWords lang docItem.atimeMs currentTime, datetimeFormat lang docItem.atimeMs )
                    |> (\( a, b ) -> ( b, a ))
    in
    div [ onClick <| ClickedDocument docItem.path, class "document-item" ]
        [ div [ class "doc-title", title docItem.path ] [ textNoTr docItem.name ]
        , div [ class "doc-opened", title lastOpenedTitle ] [ textNoTr lastOpenedString ]
        , div
            [ stopPropagationOn "click" (Dec.succeed ( ClickedRemoveDocument docItem.path, True ))
            , class "remove-button"
            , title "Remove from list"
            ]
            [ textNoTr "×" ]
        ]



-- Translation Helper Function


text : Language -> TranslationId -> Html msg
text lang tid =
    Html.text <| tr lang tid


textNoTr : String -> Html msg
textNoTr str =
    Html.text str


emptyText : Html msg
emptyText =
    Html.text ""



-- PORTS


port send : ( String, Value ) -> Cmd msg
