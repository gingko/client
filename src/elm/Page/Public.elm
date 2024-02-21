module Page.Public exposing (Model, Msg, init, navKey, update, view)

import Api
import Browser.Navigation as Nav
import Doc.Data as Data
import Doc.TreeStructure exposing (defaultTree)
import GlobalData exposing (GlobalData)
import Html exposing (Html, div)
import Html.Attributes exposing (classList, id)
import Http
import Json.Decode as Dec exposing (Decoder)
import Page.Doc
import Translation exposing (TranslationId)
import Types exposing (TooltipPosition, Tree)



-- MODEL


type alias Model =
    { title : String
    , loading : Bool
    , navKey : Nav.Key
    , doc : Page.Doc.Model
    }


init : Nav.Key -> GlobalData -> String -> ( Model, Cmd Msg )
init key globalData dbName =
    ( { title = "Public Page"
      , loading = True
      , navKey = key
      , doc = Page.Doc.init False globalData
      }
    , getPublicDocument dbName
    )



-- GETTERS


navKey : Model -> Nav.Key
navKey model =
    model.navKey



-- UPDATE


type Msg
    = NoOp
    | GotDocMsg Page.Doc.Msg
    | Keyboard String
    | TooltipRequested String TooltipPosition TranslationId
    | TooltipClosed
    | DataReceived (Result Http.Error Tree)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotDocMsg _ ->
            ( model, Cmd.none )

        Keyboard _ ->
            ( model, Cmd.none )

        TooltipRequested _ _ _ ->
            ( model, Cmd.none )

        TooltipClosed ->
            ( model, Cmd.none )

        DataReceived (Ok tree) ->
            let
                _ =
                    tree
                        |> Debug.log "tree"
            in
            ( { model
                | doc = Page.Doc.publicTreeLoaded tree model.doc
                , loading = False
              }
            , Cmd.none
            )

        DataReceived (Err err) ->
            let
                _ =
                    Debug.log "Error getting public document." err
            in
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ id "app-root", classList [ ( "loading", model.loading ) ] ]
        (Page.Doc.view
            { docMsg = GotDocMsg
            , keyboard = Keyboard
            , tooltipRequested = TooltipRequested
            , tooltipClosed = TooltipClosed
            }
            Nothing
            Nothing
            model.doc
        )



-- REQUESTS


getPublicDocument : String -> Cmd Msg
getPublicDocument dbName =
    Api.getPublicDocument DataReceived Data.publicDataDecoder dbName
