module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html
import Page.Doc
import Page.Home
import Url exposing (Url)



-- MODEL


type Model
    = Redirect Nav.Key
    | Home Page.Home.Model
    | Doc Page.Doc.Model


init : Url -> Nav.Key -> ( Model, Cmd Msg )
init url navKey =
    changeRouteTo url (Redirect navKey)


changeRouteTo : Url -> Model -> ( Model, Cmd Msg )
changeRouteTo url model =
    case url.path of
        "/" ->
            Page.Home.init (toNavKey model)
                |> updateWith Home GotHomeMsg

        dbNamePath ->
            Page.Doc.init (toNavKey model) (String.dropLeft 1 dbNamePath)
                |> updateWith Doc GotDocMsg


toNavKey : Model -> Nav.Key
toNavKey page =
    case page of
        Redirect navKey ->
            navKey

        Home home ->
            Page.Home.toNavKey home

        Doc doc ->
            Page.Doc.toNavKey doc



-- VIEW


view : Model -> Document Msg
view model =
    case model of
        Redirect _ ->
            { title = "Loading...", body = [] }

        Home homeModel ->
            { title = "Gingko - Home", body = [ Html.map GotHomeMsg (Page.Home.view homeModel) ] }

        Doc docModel ->
            { title = "Gingko", body = [ Html.map GotDocMsg (Page.Doc.view docModel) ] }



-- UPDATE


type Msg
    = ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotHomeMsg Page.Home.Msg
    | GotDocMsg Page.Doc.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ChangedUrl url, _ ) ->
            changeRouteTo url model

        ( ClickedLink _, _ ) ->
            ( model, Cmd.none )

        ( GotDocMsg docMsg, Doc docModel ) ->
            Page.Doc.update docMsg docModel
                |> updateWith Doc GotDocMsg

        ( GotHomeMsg homeMsg, Home homeModel ) ->
            Page.Home.update homeMsg homeModel
                |> updateWith Home GotHomeMsg

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Redirect _ ->
            Sub.none

        Home _ ->
            Sub.none

        Doc docModel ->
            Sub.map GotDocMsg (Page.Doc.subscriptions docModel)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }
