port module Home exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Html exposing (Html, a, button, div, h1, h2, li, ul)
import Html.Attributes exposing (class, classList, id, title)
import Html.Events exposing (onClick)
import Json.Encode as Enc
import Time
import Translation exposing (Language(..), TranslationId(..), langToString, languageDecoder, tr)


main : Program Value Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = \m -> Browser.Document "Gingko Writer - Home" (view m)
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { recentDocuments : List RecentDocument
    , language : Language
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
            ( Model [] En, Cmd.none )



-- UPDATE


type Msg
    = ClickedNew
    | ClickedOpen
    | ClickedImport
    | ClickedDocument String


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



-- VIEW


view : Model -> List (Html Msg)
view model =
    let
        docItems =
            model.recentDocuments
                |> List.sortBy (Time.posixToMillis << .atimeMs)
                |> List.reverse
                |> List.map viewDocItem
    in
    [ div [ id "container" ]
        [ div [ id "new-row" ]
            [ div
                [ onClick ClickedNew ]
                [ h2 [] [ text model.language New ]
                , div [ class "template-row" ]
                    [ a [ id "template-new", class "template-item" ]
                        [ div [ classList [ ( "template-thumbnail", True ), ( "new", True ) ] ] []
                        , div [ class "template-title" ] [ text model.language HomeBlank ]
                        ]
                    ]
                ]
            ]
        , div [ id "open-import-row" ] []
        , div [ id "recent-documents-block" ] (div [ class "list-header" ] [ text model.language RecentDocuments ] :: docItems)
        ]
    ]


viewDocItem : RecentDocument -> Html Msg
viewDocItem docItem =
    div [ onClick <| ClickedDocument docItem.path, class "document-item" ]
        [ div [ class "doc-title", title docItem.path ] [ textNoTr docItem.name ]
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
