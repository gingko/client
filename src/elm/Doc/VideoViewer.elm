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
    | CreatingCardsVideo
    | MovingCardsVideo


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
                    "https://player.vimeo.com/video/639667560?h=26f784fab2&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479"

                CreatingCardsVideo ->
                    "https://player.vimeo.com/video/639795192?h=7cc9f0a099&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479"

                MovingCardsVideo ->
                    "https://player.vimeo.com/video/640443224?h=b894587d96&amp;badge=0&amp;autopause=0&amp;player_id=0&amp;app_id=58479"
    in
    [ div [ id "video-viewer-content" ]
        [ div [ id "video-viewer-list" ]
            [ viewVideoItem "Navigation Basics (45s)" viewerState NavigationVideo |> Html.map msgWrapper
            , viewVideoItem "Writing Basics (52s)" viewerState EditingVideo |> Html.map msgWrapper
            , viewVideoItem "Creating Cards (1m20)" viewerState CreatingCardsVideo |> Html.map msgWrapper
            , viewVideoItem "Moving Cards (60s)" viewerState MovingCardsVideo |> Html.map msgWrapper
            ]
        , div [ id "video-container" ]
            [ iframe
                [ src videoSrc
                , A.attribute "frameborder" "0"
                , A.attribute "allow" "autoplay; fullscreen; picture-in-picture"
                , A.attribute "allowfullscreen" ""
                ]
                []
            ]
        ]
    ]
        |> modalWrapper modalMsg (Just "video-viewer-container") (Just [ ( "video-viewer", True ) ]) "Tutorial Videos"


viewVideoItem : String -> Model -> Model -> Html Msg
viewVideoItem videoText selectedVideo videoItem =
    div [ classList [ ( "video-item", True ), ( "active-video", videoItem == selectedVideo ) ], onClick (SetActive videoItem) ] [ text videoText ]
