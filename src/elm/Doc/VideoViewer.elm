module Doc.VideoViewer exposing (Model, Msg, init, update, view)

import Html exposing (Html, div, iframe, text)
import Html.Attributes as A exposing (classList, id, src)
import Html.Events exposing (onClick)
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


type Msg
    = SetActive Model


update : Msg -> Model
update msg =
    case msg of
        SetActive newModel ->
            newModel



-- VIEW


view : Translation.Language -> msg -> (Msg -> msg) -> Model -> List (Html msg)
view language modalMsg msgWrapper viewerState =
    let
        videoSrc =
            case viewerState of
                NavigationVideo ->
                    "https://player.vimeo.com/video/639232763?h=7a465b6f4f&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479"

                EditingVideo ->
                    "https://example.com"
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
        [ viewVideoItem "Navigation Basics" viewerState NavigationVideo |> Html.map msgWrapper
        , viewVideoItem "Writing Basics" viewerState EditingVideo |> Html.map msgWrapper
        ]
    ]
        |> modalWrapper modalMsg (Just "video-viewer-container") (Just [ ( "video-viewer", True ) ]) "Learning Videos"


viewVideoItem : String -> Model -> Model -> Html Msg
viewVideoItem videoText selectedVideo videoItem =
    div [ classList [ ( "video-item", True ), ( "active-video", videoItem == selectedVideo ) ], onClick (SetActive videoItem) ] [ text videoText ]
