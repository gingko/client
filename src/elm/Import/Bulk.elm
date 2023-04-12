module Import.Bulk exposing (decoder, encode)

import Coders exposing (treeToValue)
import Dict exposing (Dict)
import Doc.Metadata as Metadata exposing (Metadata)
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Enc
import RandomId exposing (fromObjectId)
import Types exposing (Children(..), Tree)



-- MODEL


type alias TreeEntries =
    Dict String Metadata


type alias CardEntries =
    Dict String CardData


type alias CardData =
    { treeId : String
    , parentId : Maybe String
    , position : Float
    , content : String
    }


decoder : Int -> Decoder (List ( String, Metadata, Tree ))
decoder seed =
    Dec.map2 importTrees
        (decodeTreeEntries seed)
        (decodeCardEntries seed)


encode : String -> List ( String, Metadata, Tree ) -> Enc.Value
encode author dataList =
    dataList
        |> Enc.list
            (\( tid, mdata, tree ) ->
                Enc.object
                    [ ( "id", Enc.string tid )
                    , ( "metadata", Metadata.encode mdata )
                    , ( "tree", treeToValue tree )
                    ]
            )



-- ===== INTERNAL =====


decodeTreeEntries : Int -> Decoder (Dict String Metadata)
decodeTreeEntries seed =
    let
        builder md =
            Just ( Metadata.getDocId md, md )
    in
    Metadata.decoderImport seed
        |> Dec.list
        |> Dec.map (List.filterMap (Maybe.andThen builder))
        |> Dec.map Dict.fromList
        |> Dec.field "trees"


decodeCardEntries : Int -> Decoder (Dict String CardData)
decodeCardEntries seed =
    let
        builder id tid pid pos ct del des =
            if del || des then
                Nothing

            else
                Just ( id, CardData (fromObjectId seed tid) pid pos ct )
    in
    (Dec.succeed builder
        |> required "_id" Dec.string
        |> required "treeId" Dec.string
        |> optional "parentId" (Dec.maybe Dec.string) Nothing
        |> optional "position" Dec.float 0.0
        |> optional "content" Dec.string ""
        |> optional "deleted" Dec.bool False
        |> optional "destroyed" Dec.bool False
    )
        |> Dec.list
        |> Dec.map (List.filterMap identity)
        |> Dec.map Dict.fromList
        |> Dec.field "cards"



-- Functions


importTrees : TreeEntries -> CardEntries -> List ( String, Metadata, Tree )
importTrees trees cards =
    trees
        |> Dict.toList
        |> List.map (importTree cards)


importTree : CardEntries -> ( String, Metadata ) -> ( String, Metadata, Tree )
importTree cards ( treeId, treeMeta ) =
    let
        cardsInTree =
            Dict.filter (\_ card -> card.treeId == treeId) cards
    in
    ( treeId, treeMeta, Tree treeId "" (getChildren Nothing cardsInTree) )


getChildren : Maybe String -> CardEntries -> Children
getChildren parentId_ cards =
    let
        mapFn ( id, cd ) =
            Tree id cd.content (getChildren (Just id) cards)
    in
    cards
        |> Dict.filter (\_ c -> c.parentId == parentId_)
        |> Dict.toList
        |> List.sortBy (\( _, c ) -> c.position)
        |> List.map mapFn
        |> Children
