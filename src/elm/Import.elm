module Import exposing (..)

import Dict exposing (Dict)
import Doc.TreeStructure exposing (defaultTree)
import Types exposing (Children(..), Tree)


type alias TreeEntries =
    Dict String String


type alias CardEntries =
    Dict String CardData


type alias CardData =
    { treeId : String
    , parentId : Maybe String
    , position : Float
    , content : String
    }


importTrees : TreeEntries -> CardEntries -> List ( String, Tree )
importTrees trees cards =
    trees
        |> Dict.toList
        |> List.map (importTree cards)


importTree : CardEntries -> ( String, String ) -> ( String, Tree )
importTree cards ( treeId, treeName ) =
    let
        cardsInTree =
            Dict.filter (\_ card -> card.treeId == treeId) cards
    in
    ( treeId, Tree treeId "" (getChildren Nothing cardsInTree) )


getChildren : Maybe String -> CardEntries -> Children
getChildren parentId_ cards =
    let
        mapFn ( id, cd ) =
            Tree id cd.content (getChildren (Just id) cards)
    in
    cards
        |> Dict.filter (\id c -> c.parentId == parentId_)
        |> Dict.toList
        |> List.map mapFn
        |> Children
