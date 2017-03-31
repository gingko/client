module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import Dict exposing (Dict)

import Trees


type alias Model =
  { data : Trees.Model
  , treePast : List Tree
  , treeFuture : List Tree
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  , filepath : Maybe String
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("nodes", nodesToValue model.data.nodes)
   , ("treePast", Json.Encode.list (List.map treeToValue model.treePast))
   , ("treeFuture", Json.Encode.list (List.map treeToValue model.treeFuture))
   , ("viewState", viewStateToValue model.viewState)
   , ("nextId", Json.Encode.int model.nextId)
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
    , ( "children", Json.Encode.list (List.map Json.Encode.string treeNode.children) )
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
  Json.map7 Model
    (field "tree" treesModelDecoder)
    (oneOf [field "treePast" (list treeDecoder), succeed []])
    (oneOf [field "treeFuture" (list treeDecoder), succeed []])
    (field "viewState" viewStateDecoder)
    (field "nextId" int)
    ( succeed True )
    (maybe (field "filepath" string))


treesModelDecoder : Decoder Trees.Model
treesModelDecoder =
  Json.map3 Trees.Model
    treeDecoder
    (succeed [])
    (succeed Dict.empty)


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


nodesDecoder : Decoder (Dict String TreeNode)
nodesDecoder =
  (dict treeNodeDecoder)


treeNodeDecoder : Decoder TreeNode
treeNodeDecoder =
  Json.map3 TreeNode
    (field "content" string)
    (field "children" (list string))
    (field "rev" (maybe string))
 



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
