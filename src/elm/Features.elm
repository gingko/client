module Features exposing (Feature(..), decoder)

import Json.Decode as Dec exposing (Decoder)


type Feature
    = VotingAppLinkInMenu
    | VotingAppLinkInSidebar


decoder : Decoder (List Feature)
decoder =
    Dec.oneOf
        [ Dec.list Dec.string
            |> Dec.map (List.filterMap maybeFeature)
        , Dec.succeed []
        ]


maybeFeature : String -> Maybe Feature
maybeFeature str =
    case str of
        "voting-app-link-in-menu" ->
            Just VotingAppLinkInMenu

        "voting-app-link-in-sidebar" ->
            Just VotingAppLinkInSidebar

        _ ->
            Nothing
