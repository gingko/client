module UI.Collaborators exposing (viewHeader, viewOnCard)

import Ant.Icons.Svg as AntIcons
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style, title)
import List.Extra as ListExtra
import Random
import Svg.Attributes
import Types exposing (CollabStateMode(..), Collaborator)


getColorFromIdx : Int -> ( Int, Int, Int )
getColorFromIdx idx =
    let
        allColors =
            [ ( 212, 68, 204 )
            , ( 95, 188, 53 )
            , ( 134, 77, 218 )
            , ( 153, 168, 51 )
            , ( 102, 115, 216 )
            , ( 71, 176, 96 )
            , ( 190, 92, 181 )
            , ( 210, 145, 50 )
            , ( 212, 70, 118 )
            , ( 211, 78, 51 )
            ]

        boundedIdx =
            modBy (List.length allColors) idx
                |> Debug.log "boundedIdx"
    in
    ListExtra.getAt boundedIdx allColors
        |> Maybe.withDefault ( 211, 78, 51 )


colorToRgbString : ( Int, Int, Int ) -> String
colorToRgbString ( r, g, b ) =
    "rgb(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ ")"


colorToRgbaString : Float -> ( Int, Int, Int ) -> String
colorToRgbaString a ( r, g, b ) =
    "rgba(" ++ String.fromInt r ++ "," ++ String.fromInt g ++ "," ++ String.fromInt b ++ "," ++ String.fromFloat a ++ ")"


whiteOrBlack : ( Int, Int, Int ) -> String
whiteOrBlack ( r, g, b ) =
    let
        brightness =
            (toFloat r * 299 + toFloat g * 587 + toFloat b * 114) / 1000
    in
    if brightness > 125 then
        "black"

    else
        "white"


viewHeader : List Collaborator -> Html msg
viewHeader collabs =
    div [ style "display" "flex", style "gap" "5px" ] (List.map viewCollabInHeader collabs)


viewCollabInHeader : Collaborator -> Html msg
viewCollabInHeader collab =
    let
        collabColor =
            getColorFromIdx collab.int |> colorToRgbString

        collabColorAlpha =
            getColorFromIdx collab.int |> colorToRgbaString 0.75
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
        , title collab.name
        ]
        []


viewOnCard : List Collaborator -> Html msg
viewOnCard collabs =
    span [ class "collaborators", style "display" "flex", style "gap" "5px" ]
        (List.map viewCollabSmall collabs)


viewCollabSmall : Collaborator -> Html msg
viewCollabSmall collab =
    let
        collabColor =
            getColorFromIdx collab.int |> colorToRgbString

        collabTextColor =
            getColorFromIdx collab.int |> whiteOrBlack
    in
    div
        [ style "background-color" collabColor
        , style "border-radius" "500px"
        , style "border" ("2px solid " ++ collabColor)
        , style "padding" "2px 5px"
        , style "color" collabTextColor
        , title collab.name
        ]
        [ case collab.mode of
            CollabEditing _ ->
                span
                    [ style "display" "flex"
                    , style "justify-content" "center"
                    , style "align-items" "center"
                    , style "gap" "4px"
                    ]
                    [ text collab.name
                    , AntIcons.editFilled
                        [ Svg.Attributes.width "10px"
                        , Svg.Attributes.height "10px"
                        , Svg.Attributes.fill collabTextColor
                        ]
                    ]

            CollabActive _ ->
                span [] [ text collab.name ]
        ]
