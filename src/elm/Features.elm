module Features exposing (Feature(..), decoder)

import Json.Decode as Dec exposing (Decoder)


type Feature
    = VotingAppLinkInMenu


decoder : Decoder (List Feature)
decoder =
    Dec.list Dec.string
        |> Dec.map (List.filterMap maybeFeature)


maybeFeature : String -> Maybe Feature
maybeFeature str =
    case str of
        "voting-app-link-in-menu" ->
            Just VotingAppLinkInMenu

        _ ->
            Nothing
