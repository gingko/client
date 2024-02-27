module Import.Single exposing (decoder, encode)

import Coders exposing (lazyRecurse)
import Doc.Metadata as Metadata
import Doc.TreeStructure as TreeStructure exposing (labelTree)
import Json.Decode as Dec exposing (Decoder, field, list, oneOf, string, succeed)
import Json.Encode as Enc
import Random
import RandomId
import Types exposing (Children(..), Tree)



-- EXPOSED


decoder : Random.Seed -> ( Decoder Tree, Random.Seed )
decoder seed =
    let
        ( salt, newSeed ) =
            Random.step (RandomId.stringGenerator 7) seed
    in
    ( decode
        |> Dec.map
            (\trees ->
                trees
                    |> List.indexedMap (\idx t -> labelTree idx salt t)
                    |> List.map (TreeStructure.renameNodes salt)
            )
        |> Dec.map
            (\trees ->
                Tree "0" "" (Children trees)
            )
    , newSeed
    )


encode : { author : String, docId : String, fileName : String } -> Tree -> Enc.Value
encode { author, docId, fileName } tree =
    Enc.object
        [ ( "id", Enc.string docId )
        , ( "metadata", Metadata.new docId |> Metadata.renameAndEncode fileName )
        , ( "data", Enc.null ) --TODO
        ]



-- INTERNAL


decode : Decoder (List Tree)
decode =
    Dec.list unlabelledTreeDecoder


unlabelledTreeDecoder : Decoder Tree
unlabelledTreeDecoder =
    Dec.map3 Tree
        (Dec.succeed "")
        (field "content" string)
        (oneOf
            [ field
                "children"
                (list (lazyRecurse (\_ -> unlabelledTreeDecoder))
                    |> Dec.map Children
                )
            , succeed (Children [])
            ]
        )
