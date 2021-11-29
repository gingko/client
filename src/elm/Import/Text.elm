module Import.Text exposing (toTree, view)

import Doc.TreeStructure as TreeStructure
import Html exposing (Html, button, div, input)
import Html.Attributes exposing (id, type_)
import Html.Events exposing (onClick)
import List.Extra as ListExtra
import Random
import RandomId
import Regex
import SharedUI exposing (modalWrapper)
import Types exposing (Children(..), Tree)


view : msg -> msg -> List (Html msg)
view closeMsg fileSelectorClicked =
    [ div [] [ button [ id "import-text-file-input", onClick fileSelectorClicked ] [] ] ]
        |> modalWrapper closeMsg (Just "import-text-modal") Nothing "Import Text Files"


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
