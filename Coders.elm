module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)


type alias Model =
  { contents : List Content
  , nodes : List Node
  , commits : List Commit
  , operations : List Op
  , tree : Tree
  , commit : String
  , floating : List Op
  , viewState : ViewState
  }


-- ENCODERS

modelToValue : Model -> Json.Encode.Value
modelToValue model =
  Json.Encode.object
   [ ("contents", Json.Encode.list (List.map contentToValue model.contents))
   , ("nodes", Json.Encode.list (List.map nodeToValue model.nodes))
   , ("commits", Json.Encode.list (List.map commitToValue model.commits))
   , ("operations", Json.Encode.list (List.map opToValue model.operations))
   , ("tree", treeToValue model.tree)
   , ("floating", Json.Encode.list (List.map opToValue model.floating))
   , ("commit", Json.Encode.string model.commit)
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


commitToValue : Commit -> Json.Encode.Value
commitToValue commit =
  Json.Encode.object
    [ ("_id", Json.Encode.string commit.id)
    , ("rootNode", Json.Encode.string commit.rootNode)
    , ("timestamp", Json.Encode.int commit.timestamp)
    , ("authors", Json.Encode.list (List.map Json.Encode.string commit.authors) )
    , ("committer", Json.Encode.string commit.committer)
    , ("parents", Json.Encode.list (List.map Json.Encode.string commit.parents) )
    , ("message", Json.Encode.string commit.message)
    ]


opToValue : Op -> Json.Encode.Value
opToValue op =
  case op of
    Ins id pid_ previd_ nextid_ ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Ins" )
        , ( "_id", Json.Encode.string id )
        , ( "parentId", maybeToValue pid_ Json.Encode.string )
        , ( "prevId", maybeToValue previd_ Json.Encode.string )
        , ( "nextId", maybeToValue nextid_ Json.Encode.string )
        ]

    Upd id uid str ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Upd" )
        , ( "_id", Json.Encode.string id )
        , ( "uid", Json.Encode.string uid )
        , ( "content", Json.Encode.string str )
        ]

    Del id uid ->
      Json.Encode.object
        [ ( "opType", Json.Encode.string "Del" )
        , ( "_id", Json.Encode.string id )
        , ( "uid", Json.Encode.string uid )
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



-- DECODERS

modelDecoder : Decoder Model
modelDecoder =
  Json.object8 Model
    ("contents" := Json.list contentDecoder)
    ("nodes" := Json.list nodeDecoder)
    ("commits" := Json.list commitDecoder)
    ("operations" := Json.list opDecoder)
    ("tree" := treeDecoder)
    ("commit" := string)
    ("floating" := Json.list opDecoder)
    ("viewState" := viewStateDecoder)

objectsDecoder : Decoder Objects
objectsDecoder =
  Json.object4 Objects
    ("contents" := Json.list contentDecoder)
    ("nodes" := Json.list nodeDecoder)
    ("commits" := Json.list commitDecoder)
    ("operations" := Json.list opDecoder)


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


commitDecoder : Decoder Commit
commitDecoder =
  Json.object7 Commit
    ("_id" := string)
    ("rootNode" := string)
    ("timestamp" := int)
    ("authors" := Json.list string)
    ("committer" := string)
    ("parents" := Json.list string)
    ("message" := string)


opDecoder : Decoder Op
opDecoder =
  ("opType" := string) `andThen` opInfo


opInfo : String -> Decoder Op
opInfo tag =
  case tag of
    "Ins" ->
      object4 Ins
        ("_id" := string) 
        (maybe ("parentId" := string)) 
        (maybe ("prevId" := string))
        (maybe ("nextId" := string))

    "Upd" ->
      object3 Upd 
        ("_id" := string) 
        ("uid" := string) 
        ("content" := string)

    "Del" ->
      object2 Del
        ("_id" := string) 
        ("uid" := string)

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
    ("children" := list (lazy (\_ -> treeDecoder)) |> Json.map Children )


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

lazy : (() -> Decoder a) -> Decoder a
lazy thunk =
  customDecoder value
    (\js -> decodeValue (thunk ()) js)

maybeToValue : Maybe a -> (a -> Json.Encode.Value) -> Json.Encode.Value
maybeToValue mb encoder =
  case mb of
    Nothing -> Json.Encode.null
    Just v -> encoder v
