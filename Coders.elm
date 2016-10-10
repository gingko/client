module Coders exposing (..)

import Types exposing (..)
import Json.Encode
import Json.Decode as Json exposing (..)



-- ENCODERS

opToValue : Op -> Json.Encode.Value
opToValue op =
  let
    mbToVal mb encoder =
      case mb of
        Nothing -> Json.Encode.null
        Just v -> encoder v
  in
  case op of
    Ins id pid_ previd_ nextid_ ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Ins" )
        , ( "id", Json.Encode.string id )
        , ( "parentId", mbToVal pid_ Json.Encode.string )
        , ( "prevId", mbToVal previd_ Json.Encode.string )
        , ( "nextId", mbToVal nextid_ Json.Encode.string )
        ]

    Upd id uid str ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Upd" )
        , ( "id", Json.Encode.string id )
        , ( "uid", Json.Encode.string uid )
        , ( "content", Json.Encode.string str )
        ]

    Del id uid ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Del" )
        , ( "id", Json.Encode.string id )
        , ( "uid", Json.Encode.string uid )
        ]




-- DECODERS

stateDecoder : Decoder State
stateDecoder =
  Json.object3 State
    ("objects" := objectsDecoder)
    ("commit" := string)
    ("viewState" := viewStateDecoder)


objectsDecoder : Decoder Objects
objectsDecoder =
  Json.object4 Objects
    ("contents" := Json.list contentDecoder)
    ("nodes" := Json.list nodeDecoder)
    ("commits" := Json.list commitDecoder)
    ("operations" := Json.array opDecoder)


contentDecoder : Decoder Content
contentDecoder =
  Json.object3 Content
    ("id" := string)
    ("contentType" := string)
    ("content" := string)


nodeDecoder : Decoder Node
nodeDecoder =
  Json.object3 Node
    ("id" := string)
    ("contentId" := string)
    ("childrenIds" := Json.list string)


commitDecoder : Decoder Commit
commitDecoder =
  Json.object7 Commit
    ("id" := string)
    ("rootNode" := string)
    ("timestamp" := int)
    ("authors" := Json.list string)
    ("committer" := string)
    ("parents" := Json.list string)
    ("message" := string)


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.object6 ViewState
    ("active" := string)
    ("activePast" := list string)
    ("activeFuture" := list string)
    ("descendants" := list string)
    ( maybe ("editing" := string))
    ("field" := string)
    
  

opDecoder : Decoder Op
opDecoder =
  ("type" := string) `andThen` opInfo


opInfo : String -> Decoder Op
opInfo tag =
  case tag of
    "Ins" ->
      object4 Ins
        ("id" := string) 
        (maybe ("parentId" := string)) 
        (maybe ("prevId" := string))
        (maybe ("nextId" := string))

    "Upd" ->
      object3 Upd 
        ("id" := string) 
        ("uid" := string) 
        ("content" := string)

    "Del" ->
      object2 Del
        ("id" := string) 
        ("uid" := string)

    _ -> Json.fail (tag ++ " is not a recognized type for Op")


