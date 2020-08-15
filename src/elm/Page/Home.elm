module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Doc.Metadata as Metadata exposing (Metadata)
import Html exposing (Html, a, button, div, h1, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Dec
import RandomId
import Route
import Session exposing (Session)
import Translation exposing (langFromString)



-- MODEL


type alias Model =
    { documents : List ( String, Metadata )
    , language : Translation.Language
    , session : Session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    case Session.userDb session of
        Nothing ->
            ( { documents = [], language = langFromString "en", session = session }, Cmd.none )

        Just userDb ->
            let
                rowDecoder =
                    Dec.field "value" Metadata.decoderWithDbName

                responseDecoder =
                    Dec.field "rows" (Dec.list rowDecoder)
            in
            ( { documents = [], language = langFromString "en", session = session }
            , Http.riskyRequest
                { url = "http://localhost:5984/" ++ userDb ++ "/_design/testDocList/_view/docList"
                , method = "GET"
                , body = Http.emptyBody
                , expect = Http.expectJson ReceivedDocuments responseDecoder
                , headers = []
                , timeout = Nothing
                , tracker = Nothing
                }
            )


toSession : Model -> Session
toSession model =
    model.session



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "This is the home page" ]
        , ul [] (List.map viewDocEntry model.documents)
        , button [ onClick GetNewDocId ] [ text "New" ]
        ]


viewDocEntry : ( String, Metadata ) -> Html Msg
viewDocEntry ( dbName, metadata ) =
    let
        docName =
            Metadata.getDocName metadata |> Maybe.withDefault "Untitled"
    in
    li [] [ a [ href <| "/" ++ dbName ] [ text docName ] ]



-- UPDATE


type Msg
    = ReceivedDocuments (Result Http.Error (List ( String, Metadata )))
    | GetNewDocId
    | NewDocIdReceived String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedDocuments (Ok docList) ->
            ( { model | documents = docList }, Cmd.none )

        ReceivedDocuments (Err err) ->
            case err of
                Http.BadStatus 401 ->
                    ( model, Route.replaceUrl (Session.navKey model.session) Route.Login )

                _ ->
                    ( model, Cmd.none )

        GetNewDocId ->
            ( model, RandomId.generate NewDocIdReceived )

        NewDocIdReceived docId ->
            ( model, Route.pushUrl (Session.navKey model.session) (Route.DocUntitled docId) )
