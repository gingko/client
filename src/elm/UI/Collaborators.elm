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
    div [ style "display" "flex", style "gap" "5px", style "justify-content" "center" ] (List.map viewCollabInHeader collabs)


viewCollabInHeader : Collaborator -> Html msg
viewCollabInHeader collab =
    let
        collabColor =
            getColorFromIdx collab.int |> colorToRgbString

        collabTextColor =
            getColorFromIdx collab.int |> whiteOrBlack
    in
    div
        [ style "background-color" collabColor
        , style "border-radius" "500px"
        , style "color" collabTextColor
        , style "padding" "2px 5px"
        , style "display" "inline-block"
        , style "font-size" "80%"
        ]
        [ text collab.name ]


viewOnCard : List Collaborator -> Html msg
viewOnCard collabs =
    let
        editor_ =
            collabs
                |> List.filter
                    (\c ->
                        case c.mode of
                            CollabEditing _ ->
                                True

                            _ ->
                                False
                    )
                |> List.head

        editingBorder =
            case editor_ of
                Just editor ->
                    let
                        collabColor =
                            getColorFromIdx editor.int |> colorToRgbaString 0.5
                    in
                    [ style "border" ("2px solid " ++ collabColor)
                    , style "border-left" "none"
                    , style "width" "100%"
                    , style "height" "100%"
                    , style "border-radius" "0 4px 4px 0"
                    ]

                Nothing ->
                    []
    in
    span
        ([ class "collaborators"
         , style "display" "flex"
         ]
            ++ editingBorder
        )
        (List.map viewCollabSmall collabs
            ++ (case editor_ of
                    Just editor ->
                        let
                            bgColor =
                                getColorFromIdx editor.int |> colorToRgbString
                        in
                        [ viewEditingMarker bgColor ]

                    Nothing ->
                        []
               )
        )


viewCollabSmall : { a | int : Int, mode : CollabStateMode } -> Html msg
viewCollabSmall collab =
    let
        collabColor =
            getColorFromIdx collab.int |> colorToRgbString
    in
    div
        [ style "background-color" collabColor
        , style "width" "4px"
        , style "height" "100%"
        , style "display" "flex"
        , style "justify-content" "center"
        , style "align-items" "center"
        ]
        []


viewEditingMarker : String -> Html msg
viewEditingMarker bgColor =
    div
        [ class "typing-indicator"
        , style "background-color" "white"
        ]
        (List.repeat 3 (span [ style "background-color" bgColor ] []))
