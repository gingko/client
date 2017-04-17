module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Array exposing (fromList)
import Dict exposing (Dict)

import Trees


type alias Model =
  { data : Trees.Model
  , treePast : List Tree
  , treeFuture : List Tree
  , viewState : ViewState
  , saved : Bool
  , filepath : Maybe String
  }


type alias Node =
  { id : String
  , content : String
  , children : List (String, Bool)
  , rev : Maybe String
  , deleted : Bool
  }


-- ENCODERS

nodeListToValue : List (String, TreeNode) -> Json.Encode.Value
nodeListToValue nodeList =
  Json.Encode.list
    (List.map nodeEntryToValue nodeList)


nodeEntryToValue : (String, TreeNode) -> Json.Encode.Value
nodeEntryToValue (id, n) =
  Json.Encode.object
    [ ("_id", Json.Encode.string id)
    , ("_rev", maybeToValue n.rev Json.Encode.string)
    , ("_deleted", Json.Encode.bool n.deleted)
    , ("content", Json.Encode.string n.content)
    , ( "children", Json.Encode.list
          (List.map (tupleToValue Json.Encode.string Json.Encode.bool) n.children) )
    ]


modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("nodes", nodesToValue model.data.nodes)
   , ("treePast", Json.Encode.list (List.map treeToValue model.treePast))
   , ("treeFuture", Json.Encode.list (List.map treeToValue model.treeFuture))
   , ("viewState", viewStateToValue model.viewState)
   , ("filepath", maybeToValue model.filepath Json.Encode.string )
   ]


treeToValue : Tree -> Json.Encode.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Json.Encode.object
        [ ( "id", Json.Encode.string tree.id )
        , ( "content", Json.Encode.string tree.content )
        , ( "children", Json.Encode.list (List.map treeToValue c))
        ]


viewStateToValue : ViewState -> Json.Encode.Value
viewStateToValue vs =
  Json.Encode.object
    [ ( "active", Json.Encode.string vs.active )
    , ( "activePast", Json.Encode.list (List.map Json.Encode.string vs.activePast) )
    , ( "activeFuture", Json.Encode.list (List.map Json.Encode.string vs.activeFuture) )
    , ( "descendants", Json.Encode.list (List.map Json.Encode.string vs.descendants) )
    , ( "editing", maybeToValue vs.editing Json.Encode.string )
    ]


nodesToValue : Dict String TreeNode -> Json.Encode.Value
nodesToValue nodes =
  Dict.toList nodes
    |> List.map (\(k, v) -> (k, treeNodeToValue v))
    |> Json.Encode.object


treeNodeToValue : TreeNode -> Json.Encode.Value
treeNodeToValue treeNode =
  Json.Encode.object
    [ ( "content", Json.Encode.string treeNode.content )
    , ( "children", Json.Encode.list
          (List.map (tupleToValue Json.Encode.string Json.Encode.bool) treeNode.children) )
    , ( "rev", maybeToValue treeNode.rev Json.Encode.string )
    ]




-- EXPORT ENCODINGS

treeToSimpleJSON : Tree -> Json.Encode.Value
treeToSimpleJSON tree =
  case tree.children of
    Children c ->
      Json.Encode.array 
      ( fromList
        [ Json.Encode.object
          [ ( "content", Json.Encode.string tree.content )
          , ( "children", Json.Encode.array (fromList (List.map treeToSimpleJSON c)))
          ]
        ]
      )





-- DECODERS

modelDecoder : Decoder Model
modelDecoder =
  Json.map6 Model
    (field "tree" treesModelDecoder)
    (oneOf [field "treePast" (list treeDecoder), succeed []])
    (oneOf [field "treeFuture" (list treeDecoder), succeed []])
    (field "viewState" viewStateDecoder)
    ( succeed True )
    (maybe (field "filepath" string))


treesModelDecoder : Decoder Trees.Model
treesModelDecoder =
  Json.map4 Trees.Model
    treeDecoder
    (succeed [])
    (succeed Dict.empty)
    (succeed [])


treeDecoder : Decoder Tree
treeDecoder =
  Json.map4 Tree
    (field "id" string)
    (field "content" string)
    (oneOf  [ ( field
                "children"
                ( list (lazyRecurse (\_ -> treeDecoder)) 
                  |> Json.map Children 
                )
              )
            , succeed (Children [])
            ]
    )
    (field "rev" (maybe string))


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.map5 ViewState
    (field "active" string)
    (field "activePast" (list string))
    (field "activeFuture" (list string))
    (field "descendants" (list string))
    (maybe (field "editing" string))


nodeListDecoder : Decoder (List (String, TreeNode))
nodeListDecoder =
  Json.map (\ln -> ln |> List.map (\n -> (n.id, TreeNode n.content n.children n.rev n.deleted)))
    (list nodeEntryDecoder)


nodeEntryDecoder : Decoder Node
nodeEntryDecoder =
  decode Node
    |> required "_id" string
    |> required "content" string
    |> required "children" (list (tupleDecoder string bool))
    |> required "_rev" (maybe string)
    |> optional "_deleted" bool False


nodeObjectDecoder : Decoder (String, TreeNode)
nodeObjectDecoder =
  Json.map (\n -> (n.id, TreeNode n.content n.children n.rev n.deleted))
    nodeEntryDecoder


nodesDecoder : Decoder (Dict String TreeNode)
nodesDecoder =
  (dict treeNodeDecoder)


treeNodeDecoder : Decoder TreeNode
treeNodeDecoder =
  Json.map4 TreeNode
    (field "content" string)
    (field "children" (list (tupleDecoder string bool)))
    (field "_rev" (maybe string))
    (field "_deleted" bool)




-- HELPERS

lazyRecurse : (() -> Decoder a) -> Decoder a
lazyRecurse thunk =
  let
    toResult =
      (\js -> decodeValue (thunk ()) js)
  in
  andThen
    (\a ->
      case toResult a of
        Ok b -> succeed b
        Err err -> fail err
    )
    value


maybeToValue : Maybe a -> (a -> Json.Encode.Value) -> Json.Encode.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Json.Encode.null
    Just v -> encoder v


tupleToValue : (a -> Json.Encode.Value) -> (b -> Json.Encode.Value) -> (a, b) -> Json.Encode.Value
tupleToValue aEnc bEnc (aVal, bVal) =
  Json.Encode.list [ aEnc aVal, bEnc bVal ]


tupleDecoder : Decoder a -> Decoder b -> Decoder (a, b)
tupleDecoder a b =
  index 0 a
    |> andThen
      (\aVal -> index 1 b
          |> andThen (\bVal -> succeed (aVal, bVal))
      )
