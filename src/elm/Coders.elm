module Coders exposing (..)

import Types exposing (..)
import Json.Encode as Enc
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
  { id_ : String
  , rev_ : Maybe String
  , deleted_ : Bool
  , content : String
  , children : List (String, Bool)
  , deletedWith : Maybe (List String)
  }


-- ENCODERS

nodeListToValue : List (String, TreeNode) -> Enc.Value
nodeListToValue nodeList =
  Enc.list
    (List.map nodeEntryToValue nodeList)


nodeEntryToValue : (String, TreeNode) -> Enc.Value
nodeEntryToValue (id, n) =
  Enc.object
    [ ("_id", Enc.string id)
    , ("_rev", maybeToValue n.rev Enc.string)
    , ("_deleted", Enc.bool n.deleted_)
    , ("deletedWith", maybeToValue n.deletedWith
        (\ss -> Enc.list (List.map Enc.string ss)) )
    , ("content", Enc.string n.content)
    , ( "children", Enc.list
          (List.map (tupleToValue Enc.string Enc.bool) n.children) )
    ]


modelToValue : Model -> Enc.Value
modelToValue model =
  Enc.object
   [ ("nodes", nodesToValue model.data.nodes)
   , ("treePast", Enc.list (List.map treeToValue model.treePast))
   , ("treeFuture", Enc.list (List.map treeToValue model.treeFuture))
   , ("viewState", viewStateToValue model.viewState)
   , ("filepath", maybeToValue model.filepath Enc.string )
   ]


treeToValue : Tree -> Enc.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Enc.object
        [ ( "id", Enc.string tree.id )
        , ( "content", Enc.string tree.content )
        , ( "children", Enc.list (List.map treeToValue c))
        ]


viewStateToValue : ViewState -> Enc.Value
viewStateToValue vs =
  Enc.object
    [ ( "active", Enc.string vs.active )
    , ( "activePast", Enc.list (List.map Enc.string vs.activePast) )
    , ( "activeFuture", Enc.list (List.map Enc.string vs.activeFuture) )
    , ( "descendants", Enc.list (List.map Enc.string vs.descendants) )
    , ( "editing", maybeToValue vs.editing Enc.string )
    ]


nodesToValue : Dict String TreeNode -> Enc.Value
nodesToValue nodes =
  Dict.toList nodes
    |> List.map (\(k, v) -> (k, treeNodeToValue v))
    |> Enc.object


treeNodeToValue : TreeNode -> Enc.Value
treeNodeToValue treeNode =
  Enc.object
    [ ( "content", Enc.string treeNode.content )
    , ( "children", Enc.list
          (List.map (tupleToValue Enc.string Enc.bool) treeNode.children) )
    , ( "rev", maybeToValue treeNode.rev Enc.string )
    ]




-- EXPORT ENCODINGS

treeToSimpleJSON : Tree -> Enc.Value
treeToSimpleJSON tree =
  case tree.children of
    Children c ->
      Enc.array 
      ( fromList
        [ Enc.object
          [ ( "content", Enc.string tree.content )
          , ( "children", Enc.array (fromList (List.map treeToSimpleJSON c)))
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
  Json.map (\ln -> ln |> List.map (\n -> (n.id_, TreeNode n.content n.children n.rev_ n.deletedWith n.deleted_)))
    (list nodeEntryDecoder)


nodeEntryDecoder : Decoder Node
nodeEntryDecoder =
  decode Node
    |> required "_id" string
    |> required "_rev" (maybe string)
    |> optional "_deleted" bool False
    |> required "content" string
    |> required "children" (list (tupleDecoder string bool))
    |> optional "deletedWith" (maybe (list string)) Nothing


nodeObjectDecoder : Decoder (String, TreeNode)
nodeObjectDecoder =
  Json.map (\n -> (n.id_, TreeNode n.content n.children n.rev_ n.deletedWith n.deleted_) )
    nodeEntryDecoder


saveResponseDecoder : Decoder (List ((Result ResErr ResOk), (String, TreeNode)))
saveResponseDecoder =
  (list responseEntryDecoder)


responseEntryDecoder : Decoder ((Result ResErr ResOk), (String, TreeNode))
responseEntryDecoder =
  tupleDecoder
    resDecoder
    nodeObjectDecoder


resDecoder : Decoder (Result ResErr ResOk)
resDecoder =
  Json.oneOf
    [ decode ResOk
        |> required "id" string
        |> required "ok" bool
        |> required "rev" string -- Decoder ResOk
        |> Json.map Ok
    , decode ResErr
        |> required "status" int
        |> required "name" string
        |> required "message" string
        |> required "error" bool -- Decoder ResErr
        |> Json.map Err
    ]


nodesDecoder : Decoder (Dict String TreeNode)
nodesDecoder =
  (dict treeNodeDecoder)


treeNodeDecoder : Decoder TreeNode
treeNodeDecoder =
  Json.map5 TreeNode
    (field "content" string)
    (field "children" (list (tupleDecoder string bool)))
    (field "_rev" (maybe string))
    (field "deletedWith" (maybe (list string)))
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


maybeToValue : Maybe a -> (a -> Enc.Value) -> Enc.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Enc.null
    Just v -> encoder v


tupleToValue : (a -> Enc.Value) -> (b -> Enc.Value) -> (a, b) -> Enc.Value
tupleToValue aEnc bEnc (aVal, bVal) =
  Enc.list [ aEnc aVal, bEnc bVal ]


tupleDecoder : Decoder a -> Decoder b -> Decoder (a, b)
tupleDecoder a b =
  index 0 a
    |> andThen
      (\aVal -> index 1 b
          |> andThen (\bVal -> succeed (aVal, bVal))
      )
