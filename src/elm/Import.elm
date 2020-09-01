module Import exposing (decode)

import Dict exposing (Dict)
import Doc.TreeStructure exposing (defaultTree)
import Json.Decode as Dec exposing (Decoder)
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



-- DECODER


decode : Dec.Value -> List ( String, Tree )
decode json =
    let
        decodedTrees =
            Dec.decodeValue decodeTreeEntries json

        decodedCardEntries =
            Dec.decodeValue decodeCardEntries json
    in
    case ( decodedTrees, decodedCardEntries ) of
        ( Ok treeDict, Ok cardDict ) ->
            importTrees treeDict cardDict

        ( Err treeErr, Ok cardDict ) ->
            []

        ( Ok treeDict, Err cardErr ) ->
            []

        ( Err treeErr, Err cardErr ) ->
            []



-- ===== INTERNAL =====


decodeTreeEntries : Decoder (Dict String String)
decodeTreeEntries =
    Dec.map2 (\id name -> ( fromObjectId id, name ))
        (Dec.field "_id" Dec.string)
        (Dec.field "name" Dec.string)
        |> Dec.list
        |> Dec.map Dict.fromList
        |> Dec.field "trees"


decodeCardEntries : Decoder (Dict String CardData)
decodeCardEntries =
    Dec.map5 (\id tid pid pos ct -> ( id, CardData (fromObjectId tid) pid pos ct ))
        (Dec.field "_id" Dec.string)
        (Dec.field "treeId" Dec.string)
        (Dec.field "parentId" (Dec.maybe Dec.string))
        (Dec.field "position" Dec.float)
        (Dec.field "content" Dec.string)
        |> Dec.list
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
