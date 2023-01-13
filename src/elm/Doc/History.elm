module Doc.History exposing (History, init, revert, view)

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
    , checkoutTree : Tree -> msg
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
            let
                ver_ : Maybe Version
                ver_ =
                    idxStr
                        |> String.toInt
                        |> Maybe.andThen (\idx -> Zipper.toList zipper |> List.drop idx |> List.head)
            in
            case ver_ of
                Just version ->
                    Zipper.findFirst (\ver -> ver.id == version.id) zipper
                        |> Maybe.map Zipper.current
                        |> Maybe.map .tree
                        |> Maybe.map (RemoteData.unwrap noOp checkoutTree)
                        |> Maybe.withDefault noOp

                Nothing ->
                    noOp
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
