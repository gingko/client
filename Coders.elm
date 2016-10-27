module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import String
import TreeUtils exposing (newLine)


type alias Model =
  { trees : List Tree
  , viewState : ViewState
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("trees", Json.Encode.list (List.map treeToValue model.trees))
   , ("viewState", viewStateToValue model.viewState)
   ]


opToValue : Op -> Json.Encode.Value
opToValue op =
  case op of
    Ins id content pid_ position upd_ ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Ins" )
        , ( "_id", Json.Encode.string id )
        , ( "content", Json.Encode.string content)
        , ( "parentId", maybeToValue pid_ Json.Encode.string )
        , ( "position", Json.Encode.int position )
        , ( "upd", maybeToValue upd_ Json.Encode.string )
        ]

    Del id ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Del" )
        , ( "_id", Json.Encode.string id )
        ]



flopToValue : (Op, Bool) -> Json.Encode.Value
flopToValue flop =
  Json.Encode.list
    [ opToValue (fst flop)
    , Json.Encode.bool (snd flop)
    ]


treeToValue : Tree -> Json.Encode.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Json.Encode.object
        [ ( "id", Json.Encode.string tree.id )
        , ( "content", Json.Encode.string tree.content )
        , ( "parentId", maybeToValue tree.parentId Json.Encode.string )
        , ( "position", Json.Encode.int tree.position )
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
  Json.object2 Model
    ("trees" := Json.list treeDecoder)
    ("viewState" := viewStateDecoder)


opDecoder : Decoder Op
opDecoder =
  ("opType" := string) `andThen` opInfo


opInfo : String -> Decoder Op
opInfo tag =
  case tag of
    "Ins" ->
      object5 Ins
        ("_id" := string) 
        ("content" := string)
        (maybe ("parentId" := string)) 
        ("position" := int) 
        (maybe ("upd" := string))

    "Del" ->
      object1 Del
        ("_id" := string) 

    _ -> Json.fail (tag ++ " is not a recognized type for Op")


treeDecoder : Decoder Tree
treeDecoder =
  Json.object5 Tree
    ("id" := string)
    ("content" := string)
    (maybe ("parentId" := string)) 
    ("position" := int)
    ("children" := list (lazyRecurse (\_ -> treeDecoder)) |> Json.map Children )


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
