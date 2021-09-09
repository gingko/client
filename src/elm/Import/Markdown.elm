module Import.Markdown exposing (toTree)

import Doc.TreeStructure as TreeStructure
import List.Extra as ListExtra
import Random
import RandomId
import Regex
import Types exposing (Children(..), Tree)


toTree : Random.Seed -> List String -> List String -> ( Tree, Random.Seed )
toTree seed metadata markdownStrings =
    let
        ( salt, newSeed ) =
            Random.step RandomId.stringGenerator seed

        filenameAndContent =
            ListExtra.zip metadata markdownStrings

        removeExtensionRegex_ =
            Regex.fromString "\\..*$"

        titleContentToString title content =
            case removeExtensionRegex_ of
                Just regex ->
                    "# " ++ (title |> Regex.replace regex (\_ -> "")) ++ "\n" ++ content

                Nothing ->
                    "# " ++ title ++ "\n" ++ content

        mapFn idx ( title, content ) =
            Tree (String.fromInt (idx + 1))
                (titleContentToString title content)
                (Children [])

        newTree =
            filenameAndContent
                |> List.indexedMap mapFn
                |> Children
                |> Tree "0" ""
                |> TreeStructure.renameNodes salt
    in
    ( newTree, newSeed )
