module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)
import Array exposing (fromList)
import String
import TreeUtils exposing (newLine)


type alias Model =
  { contents : List Content
  , nodes : List Node
  , trees : List Tree
  , viewState : ViewState
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("contents", Json.Encode.list (List.map contentToValue model.contents))
   , ("nodes", Json.Encode.list (List.map nodeToValue model.nodes))
   , ("trees", Json.Encode.list (List.map treeToValue model.trees))
   , ("viewState", viewStateToValue model.viewState)
   ]


contentToValue : Content -> Json.Encode.Value
contentToValue content =
  Json.Encode.object
    [ ("_id", Json.Encode.string content.id)
    , ("contentType", Json.Encode.string content.contentType)
    , ("content", Json.Encode.string content.content)
    ]


nodeToValue : Node -> Json.Encode.Value
nodeToValue node =
  Json.Encode.object
    [ ("_id", Json.Encode.string node.id)
    , ("contentId", Json.Encode.string node.contentId)
    , ("childrenIds", Json.Encode.list (List.map Json.Encode.string node.childrenIds) )
    ]


opToValue : Op -> Json.Encode.Value
opToValue op =
  case op of
    Ins id content pid_ position upd_ ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Ins" )
        , ( "_id", Json.Encode.string id )
        , ( "content", contentToValue content)
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
        [ ( "uid", Json.Encode.string tree.uid )
        , ( "content", contentToValue tree.content )
        , ( "parentId", maybeToValue tree.parentId Json.Encode.string )
        , ( "prev", maybeToValue tree.prev Json.Encode.string )
        , ( "next", maybeToValue tree.next Json.Encode.string )
        , ( "visible", Json.Encode.bool tree.visible )
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
          [ ( "content", contentToString tree.content )
          , ( "children", Json.Encode.array (fromList (List.map treeToSimpleJSON c)))
          ]
        ]
      )


contentToString : Content -> Json.Encode.Value
contentToString content =
  Json.Encode.string content.content




-- DECODERS

modelDecoder : Decoder Model
modelDecoder =
  Json.object4 Model
    ("contents" := Json.list contentDecoder)
    ("nodes" := Json.list nodeDecoder)
    ("trees" := Json.list treeDecoder)
    ("viewState" := viewStateDecoder)


contentDecoder : Decoder Content
contentDecoder =
  Json.object3 Content
    ("_id" := string)
    ("contentType" := string)
    ("content" := string)


nodeDecoder : Decoder Node
nodeDecoder =
  Json.object3 Node
    ("_id" := string)
    ("contentId" := string)
    ("childrenIds" := Json.list string)


opDecoder : Decoder Op
opDecoder =
  ("opType" := string) `andThen` opInfo


opInfo : String -> Decoder Op
opInfo tag =
  case tag of
    "Ins" ->
      object5 Ins
        ("_id" := string) 
        ("content" := contentDecoder)
        (maybe ("parentId" := string)) 
        ("position" := int) 
        (maybe ("upd" := string))

    "Del" ->
      object1 Del
        ("_id" := string) 

    _ -> Json.fail (tag ++ " is not a recognized type for Op")


treeDecoder : Decoder Tree
treeDecoder =
  Json.object7 Tree
    ("uid" := string)
    ("content" := contentDecoder)
    (maybe ("parentId" := string)) 
    (maybe ("prev" := string))
    (maybe ("next" := string))
    ("visible" := bool)
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
