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
    Ins pid_ previd_ nextid_ ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Ins" )
        , ( "parentId", mbToVal pid_ Json.Encode.string )
        , ( "prevId", mbToVal previd_ Json.Encode.string )
        , ( "nextId", mbToVal nextid_ Json.Encode.string )
        ]

    Upd id str ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Upd" )
        , ( "id", Json.Encode.string id )
        , ( "content", Json.Encode.string str )
        ]

    Del id ->
      Json.Encode.object
        [ ( "type", Json.Encode.string "Del" )
        , ( "id", Json.Encode.string id )
        ]




-- DECODERS

opDecoder : Decoder Op
opDecoder =
  ("type" := string) `andThen` opInfo

opInfo : String -> Decoder Op
opInfo tag =
  case tag of
    "Ins" ->
      object3 Ins
        (maybe ("parentId" := string)) 
        (maybe ("prevId" := string))
        (maybe ("nextId" := string))

    "Upd" ->
      object2 Upd 
        ("id" := string) 
        ("content" := string)

    "Del" ->
      object1 Del ("id" := string)

    _ -> Json.fail (tag ++ " is not a recognized type for Op")


