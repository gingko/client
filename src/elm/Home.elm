port module Home exposing (main)

import Browser
import Codec exposing (Codec, Value)
import Html exposing (Html, button, h1, li, text, ul)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Json.Encode as Enc
import Time


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
    { recentDocuments : List RecentDocument }


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
            let
                _ =
                    Debug.log "home init err" err
            in
            ( Model [], Cmd.none )



-- UPDATE


type Msg
    = ClickedNew
    | ClickedOpen
    | ClickedDocument String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedNew ->
            ( model, send ( "ClickedNew", Enc.null ) )

        ClickedOpen ->
            ( model, send ( "ClickedOpen", Enc.null ) )

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
    [ button [ id "new-doc-button", onClick ClickedNew ] [ text "New Gingko Document" ]
    , button [ id "open-doc-button", onClick ClickedOpen ] [ text "Open Gingko Document" ]
    , ul [] docItems
    ]


viewDocItem : RecentDocument -> Html Msg
viewDocItem docItem =
    li [ onClick <| ClickedDocument docItem.path ] [ text docItem.name ]



-- PORTS


port send : ( String, Value ) -> Cmd msg
