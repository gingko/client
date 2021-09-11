module Import.Single exposing (decoder, encode)

import Coders exposing (lazyRecurse)
import Doc.Data as Data
import Doc.Metadata as Metadata
import Doc.TreeStructure as TreeStructure exposing (labelTree)
import Json.Decode as Dec exposing (Decoder, field, list, oneOf, string, succeed)
import Json.Encode as Enc
import Random
import RandomId
import Types exposing (Children(..), Tree)



-- MODEL


type alias NumberedTree =
    { id : String
    , content : String
    , children : NumberedChildren
    }


type NumberedChildren
    = NumberedChildren (List NumberedTree)



-- EXPOSED


decoder : Random.Seed -> ( Decoder Tree, Random.Seed )
decoder seed =
    let
        ( salt, newSeed ) =
            Random.step RandomId.stringGenerator seed
    in
    ( decode
        |> Dec.map (\ult -> labelTree 0 "" ult)
        |> Dec.map (TreeStructure.renameNodes salt)
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


decode : Decoder Tree
decode =
    Dec.list unlabelledTreeDecoder
        |> Dec.map (\children -> Tree "" "" (Children children))


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
