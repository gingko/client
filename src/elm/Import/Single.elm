module Import.Single exposing (decoder, encode)

import Coders exposing (lazyRecurse)
import Doc.Data as Data
import Doc.Metadata as Metadata
import Doc.TreeStructure as TreeStructure
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
    ( decodeNumbered
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


labelTree : Int -> String -> NumberedTree -> Tree
labelTree idx pid ult =
    let
        newId =
            pid ++ "." ++ String.fromInt idx
    in
    case ult.children of
        NumberedChildren [] ->
            Tree newId ult.content (Children [])

        NumberedChildren childs ->
            Tree
                newId
                ult.content
                (Children
                    (childs
                        |> List.indexedMap (\i ut -> labelTree i newId ut)
                    )
                )


decodeNumbered : Decoder NumberedTree
decodeNumbered =
    Dec.list unlabelledTreeDecoder
        |> Dec.map (\children -> NumberedTree "" "" (NumberedChildren children))


unlabelledTreeDecoder : Decoder NumberedTree
unlabelledTreeDecoder =
    Dec.map3 NumberedTree
        (Dec.succeed "")
        (field "content" string)
        (oneOf
            [ field
                "children"
                (list (lazyRecurse (\_ -> unlabelledTreeDecoder))
                    |> Dec.map NumberedChildren
                )
            , succeed (NumberedChildren [])
            ]
        )
