module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Json.Decode as Json
import Page.Doc
import Page.Home
import Url exposing (Url)



-- MODEL


type Model
    = Home Page.Home.Model
    | Doc Page.Doc.Model


init : ( Json.Value, Page.Doc.InitModel, Bool ) -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( docModel, docCmd ) =
            Page.Doc.init flags
    in
    case url.path of
        "/" ->
            ( Home Page.Home.init, Cmd.none )

        _ ->
            ( Doc docModel, Cmd.map GotDocMsg docCmd )



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Home homeModel ->
            { title = "Gingko - Home", body = [ Page.Home.view homeModel ] }

        Doc docModel ->
            { title = "Gingko", body = [ Html.map GotDocMsg (Page.Doc.view docModel) ] }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotDocMsg Page.Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl _, _ ) ->
            ( model, Cmd.none )

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )

        ( GotDocMsg docMsg, Doc docModel ) ->
            let
                ( newDocModel, docCmd ) =
                    Page.Doc.update docMsg docModel
            in
            ( Doc newDocModel, Cmd.map GotDocMsg docCmd )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Doc docModel ->
            Sub.map GotDocMsg (Page.Doc.subscriptions docModel)

        Home _ ->
            Sub.none



-- MAIN


main : Program ( Json.Value, Page.Doc.InitModel, Bool ) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
