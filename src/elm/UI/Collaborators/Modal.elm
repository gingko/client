module UI.Collaborators.Modal exposing (..)

-- MODEL

import Ant.Icons.Svg as AntIcon
import Html exposing (Html, br, div, hr, img, input, span, strong, text)
import Html.Attributes exposing (autofocus, class, id, placeholder, src, type_)
import Html.Events exposing (onClick, onInput)
import Svg.Attributes as SA exposing (height, width)
import Translation
import Utils


type alias Model =
    { isOwner : Bool
    , collabs : List String
    , newCollabField : String
    }


init : String -> List String -> Model
init myEmail collabs =
    { isOwner = not <| List.member myEmail collabs
    , collabs = collabs
    , newCollabField = ""
    }



-- UPDATE


type Msg
    = AddCollabFieldUpdated String


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddCollabFieldUpdated newCollabField ->
            { model | newCollabField = newCollabField }



-- VIEW


view : { toSelf : Msg -> msg, addCollab : String -> msg, removeCollab : String -> msg } -> Translation.Language -> Model -> List (Html msg)
view msgs lang model =
    if model.isOwner then
        [ div [] (List.map (viewCollab msgs.removeCollab) model.collabs)
        , div [ class "flex-row", class "gap-2", class "mt-3" ]
            [ input
                [ id "add-collab-input"
                , placeholder "New collaborator's email"
                , class "w-2/3"
                , class "text-base"
                , class "px-2"
                , type_ "email"
                , autofocus True
                , onInput (msgs.toSelf << AddCollabFieldUpdated)
                ]
                []
            , div
                [ class "w-1/3"
                , class "py-2"
                , class "bg-blue-400"
                , class "rounded-md"
                , class "text-white"
                , class "flex"
                , class "items-center"
                , class "justify-center"
                , class "cursor-pointer"
                , onClick (msgs.addCollab model.newCollabField)
                ]
                [ text "Grant access" ]
            ]
        , hr [ class "w-full" ] []
        , betaWarning lang
        ]

    else
        [ div [ class "pt-2", class "pb-5" ]
            [ text "You can edit this document, but you cannot manage who can access it."
            , br [] []
            , text "Contact the owner of the document if you need to add or remove collaborators."
            ]
        ]


viewCollab : (String -> msg) -> String -> Html msg
viewCollab removeMsg email =
    div [ class "flex", class "items-center", class "gap-2", class "mb-2" ]
        [ img [ src (Utils.gravatar 22 email) ] []
        , span [ class "cursor-default" ] [ text email ]
        , AntIcon.closeCircleFilled
            [ width "16px"
            , height "16px"
            , SA.class "cursor-pointer"
            , onClick (removeMsg email)
            ]
        ]


betaWarning : Translation.Language -> Html msg
betaWarning lang =
    div
        [ class "bg-amber-100"
        , class "border-amber-400"
        , class "rounded-md"
        , class "flex"
        , class "gap-4"
        , class "p-4"
        , class "text-amber-900"
        , class "fill-orange-600"
        , class "items-center"
        , class "mt-6"
        , class "text-sm"
        ]
        [ AntIcon.warningFilled [ width "20px", height "20px" ]
        , div []
            [ text "Realtime Collaboration is "
            , strong [] [ text "currently in beta" ]
            , text "."
            , br [] []
            , text "Please back up your document regularly while testing this feature."
            ]
        ]
