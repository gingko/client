module Import.Markdown exposing (toTree)

import Doc.TreeStructure as TreeStructure
import Random
import RandomId
import Types exposing (Children(..), Tree)


toTree : Random.Seed -> List String -> ( Tree, Random.Seed )
toTree seed markdownStrings =
    let
        ( salt, newSeed ) =
            Random.step RandomId.stringGenerator seed

        newTree =
            markdownStrings
                |> List.indexedMap (\idx s -> Tree (String.fromInt (idx + 1)) s (Children []))
                |> Children
                |> Tree "0" ""
                |> TreeStructure.renameNodes salt
    in
    ( newTree, newSeed )
