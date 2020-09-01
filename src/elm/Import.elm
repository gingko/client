module Import exposing (decoder, encode)

import Dict exposing (Dict)
import Doc.Data as Data exposing (Data)
import Json.Decode as Dec exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Enc
import RandomId exposing (fromObjectId)
import Types exposing (Children(..), Tree)



-- MODEL


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


decoder : String -> Decoder (List ( String, Data ))
decoder author =
    Dec.map2 importTrees
        decodeTreeEntries
        decodeCardEntries
        |> Dec.map (toData author)


encode : List ( String, Data ) -> Enc.Value
encode dataList =
    dataList
        |> Enc.list (\( tid, tdata ) -> Enc.object [ ( "treeId", Enc.string tid ), ( "data", Data.encodeData tdata ) ])



-- ===== INTERNAL =====


decodeTreeEntries : Decoder (Dict String String)
decodeTreeEntries =
    (Dec.succeed (\id name -> ( fromObjectId id, name ))
        |> required "_id" Dec.string
        |> required "name" Dec.string
    )
        |> Dec.list
        |> Dec.map Dict.fromList
        |> Dec.field "trees"


decodeCardEntries : Decoder (Dict String CardData)
decodeCardEntries =
    let
        builder id tid pid pos ct del des =
            if del || des then
                Nothing

            else
                Just ( id, CardData (fromObjectId tid) pid pos ct )
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


toData : String -> List ( String, Tree ) -> List ( String, Data )
toData author trees =
    trees
        |> List.map (\( tid, tree ) -> ( tid, Data.commitTree author [] 0 tree Data.emptyData ))
