module Doc.History exposing (History, checkoutVersion, getCurrentVersion, init, revert, view)

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


type History
    = History Tree (Zipper Version)
    | Empty


type alias Version =
    { id : String
    , timestamp : Time.Posix
    , tree : WebData Tree
    }


init : Tree -> Data.Model -> History
init tree data =
    data
        |> Data.getHistoryList
        |> List.map (\( id, timestamp, tree_ ) -> { id = id, timestamp = timestamp, tree = tree_ |> RemoteData.fromMaybe (Http.BadBody "") })
        |> Zipper.fromList
        |> Maybe.map (History tree)
        |> Maybe.withDefault Empty


checkoutVersion : String -> History -> Maybe ( History, Tree )
checkoutVersion id history =
    case history of
        History origTree zipper ->
            zipper
                |> Zipper.findFirst (\v -> v.id == id)
                |> Maybe.map (\z -> ( History origTree z, Zipper.current z |> .tree ))
                |> Maybe.andThen (\( h, tree ) -> tree |> RemoteData.toMaybe |> Maybe.map (\t -> ( h, t )))

        Empty ->
            Nothing


getCurrentVersion : History -> Maybe String
getCurrentVersion history =
    case history of
        History _ zipper ->
            zipper
                |> Zipper.current
                |> .id
                |> Just

        Empty ->
            Nothing


revert : History -> Maybe Tree
revert model =
    case model of
        History tree _ ->
            Just tree

        Empty ->
            Nothing



-- VIEW


type alias ViewConfig msg =
    { lang : Translation.Language
    , noOp : msg
    , checkoutTree : String -> msg
    , restore : msg
    , cancel : msg
    , tooltipRequested : String -> TooltipPosition -> TranslationId -> msg
    , tooltipClosed : msg
    }


view : ViewConfig msg -> History -> Html msg
view config model =
    case model of
        History _ zipper ->
            viewHistory config zipper

        Empty ->
            div [ id "history-menu" ] [ textNoTr "No history" ]


viewHistory : ViewConfig msg -> Zipper Version -> Html msg
viewHistory { lang, noOp, checkoutTree, restore, cancel, tooltipRequested, tooltipClosed } zipper =
    let
        maybeTimeDisplay =
            textNoTr ""

        maxIdx =
            Zipper.toList zipper
                |> List.length
                |> (\len -> len - 1)
                |> String.fromInt

        maybeCheckoutTree : String -> msg
        maybeCheckoutTree idxStr =
            idxStr
                |> String.toInt
                |> Maybe.andThen (\idx -> Zipper.toList zipper |> List.drop idx |> List.head)
                |> Maybe.map .id
                |> Maybe.map checkoutTree
                |> Maybe.withDefault noOp
    in
    div [ id "history-menu" ]
        [ input [ id "history-slider", type_ "range", A.min "0", A.max maxIdx, step "1", onInput maybeCheckoutTree ] []
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
