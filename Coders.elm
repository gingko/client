module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import String
import TreeUtils exposing (newLine)


type alias Model =
  { tree : Tree
  , treePast : List Tree
  , treeFuture : List Tree
  , viewState : ViewState
  , nextId : Int
  , saved : Bool
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("tree", treeToValue model.tree)
   , ("treePast", Json.Encode.list (List.map treeToValue model.treePast))
   , ("treeFuture", Json.Encode.list (List.map treeToValue model.treeFuture))
   , ("viewState", viewStateToValue model.viewState)
   , ("nextId", Json.Encode.int model.nextId)
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
    , ( "field", Json.Encode.string vs.active )
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
  Json.object6 Model
    ("tree" := treeDecoder)
    (oneOf ["treePast" := list treeDecoder, succeed []])
    (oneOf ["treeFuture" := list treeDecoder, succeed []])
    ("viewState" := viewStateDecoder)
    ("nextId" := int)
    ( succeed True )


treeDecoder : Decoder Tree
treeDecoder =
  Json.object3 Tree
    ("id" := string)
    ("content" := string)
    (oneOf  [ ("children" := list (lazyRecurse (\_ -> treeDecoder)) |> Json.map Children )
            , succeed (Children [])
            ]
    )


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.object6 ViewState
    ("active" := string)
    ("activePast" := list string)
    ("activeFuture" := list string)
    ("descendants" := list string)
    ( maybe ("editing" := string))
    ("field" := string)
    
  



-- HELPERS

lazyRecurse : (() -> Decoder a) -> Decoder a
lazyRecurse thunk =
  customDecoder value
    (\js -> decodeValue (thunk ()) js)

maybeToValue : Maybe a -> (a -> Json.Encode.Value) -> Json.Encode.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Json.Encode.null
    Just v -> encoder v
