module Doc.VideoViewer exposing (Model, init, view)

import Html exposing (Html, div, iframe, text)
import Html.Attributes as A exposing (classList, id, src)
import SharedUI exposing (modalWrapper)
import Translation



-- MODEL


type Model
    = NavigationVideo
    | EditingVideo


init : Model
init =
    NavigationVideo



-- UPDATE
-- VIEW


view : Translation.Language -> msg -> Model -> List (Html msg)
view language modalMsg viewerState =
    let
        videoSrc =
            case viewerState of
                NavigationVideo ->
                    "https://player.vimeo.com/video/639232763?h=7a465b6f4f&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479"

                EditingVideo ->
                    "https://notfound.com"
    in
    [ div [ id "video-viewer-content" ]
        [ iframe
            [ src videoSrc
            , A.attribute "frameborder" "0"
            , A.attribute "allow" "autoplay; fullscreen; picture-in-picture"
            , A.attribute "allowfullscreen" ""
            ]
            []
        ]
    , div [ id "video-viewer-list" ]
        [ div [ classList [ ( "video-item", True ), ( "active-video", viewerState == NavigationVideo ) ] ] [ text "Navigation Basics" ]
        , div [ classList [ ( "video-item", True ), ( "active-video", viewerState == EditingVideo ) ] ] [ text "Writing Basics" ]
        ]
    ]
        |> modalWrapper modalMsg (Just "video-viewer-container") (Just [ ( "video-viewer", True ) ]) "Learning Videos"
