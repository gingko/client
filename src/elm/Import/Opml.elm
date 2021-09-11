module Import.Opml exposing (treeResult)

import Doc.TreeStructure as TreeStructure exposing (labelTree)
import Random
import RandomId
import Types exposing (Children(..), Tree)
import Xml.Decode as Xml


treeResult : Random.Seed -> String -> ( Result String Tree, Random.Seed )
treeResult seed importedString =
    let
        ( salt, newSeed ) =
            Random.step RandomId.stringGenerator seed
    in
    case Xml.run opmlDecoder importedString of
        Ok trees ->
            let
                newTree =
                    Tree "0" "" (Children trees)
                        |> labelTree 0 ""
                        |> TreeStructure.renameNodes salt
            in
            ( Ok newTree, newSeed )

        Err e ->
            ( Err e, newSeed )


outlineDecoder : Xml.Decoder Tree
outlineDecoder =
    Xml.map2 (Tree "0")
        (Xml.stringAttr "text")
        (Xml.oneOf
            [ Xml.path [ "outline" ]
                (Xml.list (Xml.lazy (\_ -> outlineDecoder)))
                |> Xml.map Children
            , Xml.succeed (Children [])
            ]
        )


opmlDecoder : Xml.Decoder (List Tree)
opmlDecoder =
    Xml.path [ "body", "outline" ] (Xml.list outlineDecoder)
