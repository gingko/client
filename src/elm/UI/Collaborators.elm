module UI.Collaborators exposing (viewHeader, viewOnCard)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Random
import Svg.Attributes
import Types exposing (CollabState, CollabStateMode(..))



-- Function to get the numeric value of a base36 character


charValue : Char -> Maybe Int
charValue char =
    if '0' <= char && char <= '9' then
        Just (Char.toCode char - Char.toCode '0')

    else if 'a' <= char && char <= 'z' then
        Just (10 + Char.toCode char - Char.toCode 'a')

    else
        Nothing



-- Function to convert a base36 string to a number


base36ToNumber : List Char -> Maybe Int
base36ToNumber chars =
    let
        folder char ( index, acc ) =
            case charValue char of
                Just value ->
                    ( index + 1, acc + value * (36 ^ index) )

                Nothing ->
                    ( 0, 0 )

        -- Invalid character found, reset
    in
    case List.foldl folder ( 0, 0 ) (List.reverse chars) of
        ( _, 0 ) ->
            Nothing

        -- Invalid base36 string
        ( _, sum ) ->
            Just sum


getColor : CollabState -> ( Int, Int, Int )
getColor collabState =
    let
        uid =
            String.toList collabState.uid

        uidTail =
            List.drop (List.length uid - 5) uid

        uidTailNumber =
            case base36ToNumber uidTail of
                Just number ->
                    number

                Nothing ->
                    0

        seed =
            Random.initialSeed uidTailNumber

        ( rgb, _ ) =
            Random.step randomColor seed
    in
    rgb


randomColor : Random.Generator ( Int, Int, Int )
randomColor =
    Random.map3 (\r g b -> ( r, g, b ))
        (Random.int 0 255)
        (Random.int 0 255)
        (Random.int 0 255)


colorToRgbString : ( Int, Int, Int ) -> String
colorToRgbString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


colorToRgbaString : Float -> ( Int, Int, Int ) -> String
colorToRgbaString a ( r, g, b ) =
    "rgba(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ "," ++ String.fromFloat a ++ ")"


viewHeader : List CollabState -> Html msg
viewHeader collabs =
    div [ style "display" "flex", style "gap" "5px" ] (List.map viewCollab collabs)


viewCollab : CollabState -> Html msg
viewCollab collab =
    let
        collabColor =
            getColor collab |> colorToRgbString

        collabColorAlpha =
            getColor collab |> colorToRgbaString 0.5
    in
    div
        [ style "background-color" collabColorAlpha
        , style "border-color" collabColor
        , style "border-radius" "50%"
        , style "border-width" "3px"
        , style "border-style" "solid"
        , style "width" "20px"
        , style "height" "20px"
        , style "display" "inline-block"
        ]
        []


viewOnCard : List CollabState -> Html msg
viewOnCard collabs =
    span [ class "collaborators", style "display" "flex", style "gap" "5px" ]
        (List.map viewCollabSmall collabs)


viewCollabSmall : CollabState -> Html msg
viewCollabSmall collab =
    let
        collabColor =
            getColor collab |> colorToRgbString
    in
    div
        [ style "background-color" collabColor
        , style "border-radius" "50%"
        , style "width" "16px"
        , style "height" "16px"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        [ case collab.mode of
            CollabEditing _ ->
                AntIcons.editFilled
                    [ Svg.Attributes.width "10px"
                    , Svg.Attributes.height "10px"
                    ]

            CollabActive _ ->
                text ""
        ]
