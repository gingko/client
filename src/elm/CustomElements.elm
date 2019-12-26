module CustomElements exposing (gitgraph)

import Dict
import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (attribute, id, property, style)
import Json.Encode as Json exposing (Value, list, object, string)
import Objects exposing (Model)
import Types exposing (Msg)


commitsToValue : Model -> Value
commitsToValue objects =
    objects.commits
        |> Dict.toList
        |> List.sortBy (\( k, v ) -> -v.timestamp)
        |> List.map (\( k, v ) -> { sha = k, parents = v.parents })
        |> Json.list
            (\c ->
                object [ ( "sha", string c.sha ), ( "parents", list string c.parents ) ]
            )


commitsGraph : Model -> Html a
commitsGraph commits =
    Html.node "git-graph"
        [ property "commits" (commits |> commitsToValue)
        , attribute "commits" (commits |> commitsToValue |> Json.encode 0)
        ]
        []


gitgraph : Model -> Html Msg
gitgraph objects =
    div [ id "history-graph" ]
        [ commitsGraph objects ]
