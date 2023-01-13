module Doc.History exposing (Model, init, revert, view)

import Ant.Icons.Svg as AntIcons
import Doc.Data as Data
import Html exposing (Html, button, div, input)
import Html.Attributes as A exposing (id, step, type_)
import Html.Events exposing (onClick, onInput, onMouseEnter, onMouseLeave)
import Http
import List.Zipper as Zipper exposing (Zipper)
import RemoteData exposing (WebData)
import Time
import Translation exposing (TranslationId(..))
import Types exposing (TooltipPosition(..), Tree)
import Utils exposing (text, textNoTr)



-- MODEL


type Model
    = History Tree (Zipper Version)
    | Empty


type alias Version =
    { id : String
    , timestamp : Time.Posix
    , tree : WebData Tree
    }


init : Tree -> Data.Model -> Model
init tree data =
    Empty


fromList : ( String, Tree ) -> List ( String, Time.Posix, Maybe Tree ) -> Model
fromList ( selectedVersion, originalTree ) versionTuples =
    versionTuples
        |> List.map (\( id, timestamp, tree_ ) -> { id = id, timestamp = timestamp, tree = tree_ |> RemoteData.fromMaybe (Http.BadBody <| "Commit" ++ selectedVersion ++ " not found") })
        |> Zipper.fromList
        |> Maybe.andThen (Zipper.find (\version -> version.id == selectedVersion))
        |> Maybe.map (History originalTree)
        |> Maybe.withDefault Empty


revert : Model -> Maybe Tree
revert model =
    case model of
        History tree _ ->
            Just tree

        Empty ->
            Nothing



-- UPDATE


type Msg
    = SelectVersion String



-- VIEW


type alias ViewConfig msg =
    { lang : Translation.Language
    , toSelf : Msg -> msg
    , restore : msg
    , cancel : msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }


view : ViewConfig msg -> Model -> Html msg
view config model =
    case model of
        History _ zipper ->
            viewHistory config zipper

        Empty ->
            div [ id "history-menu" ] [ textNoTr "No history" ]


viewHistory : ViewConfig msg -> Zipper Version -> Html msg
viewHistory { lang, toSelf, restore, cancel, tooltipRequested, tooltipClosed } zipper =
    let
        maybeTimeDisplay =
            textNoTr ""

        maxIdx =
            Zipper.toList zipper
                |> List.length
                |> (\len -> len - 1)
                |> String.fromInt
    in
    div [ id "history-menu" ]
        [ input [ id "history-slider", type_ "range", A.min "0", A.max maxIdx, step "1", onInput <| (toSelf << SelectVersion) ] []
        , maybeTimeDisplay
        , button [ id "history-restore", onClick restore ] [ text lang RestoreThisVersion ]
        , div
            [ id "history-close-button"
            , onClick cancel
            , onMouseEnter <| tooltipRequested "history-close-button" BelowLeftTooltip Cancel
            , onMouseLeave <| tooltipClosed
            ]
            [ AntIcons.closeOutlined [] ]
        ]
