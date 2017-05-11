module Coders exposing (..)

import Trees
import Types exposing (..)
import Json.Encode as Enc
import Json.Decode as Json exposing (..)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Array exposing (fromList)
import Dict exposing (Dict)




-- Tree

treeToValue : Tree -> Enc.Value
treeToValue tree =
  case tree.children of
    Children c ->
      Enc.object
        [ ( "id", Enc.string tree.id )
        , ( "content", Enc.string tree.content )
        , ( "children", Enc.list (List.map treeToValue c))
        ]


treesModelDecoder : Decoder Trees.Model
treesModelDecoder =
  Json.map2 Trees.Model
    treeDecoder
    (succeed [])


treeDecoder : Decoder Tree
treeDecoder =
  Json.map3 Tree
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




-- ViewState

viewStateToValue : ViewState -> Enc.Value
viewStateToValue vs =
  Enc.object
    [ ( "active", Enc.string vs.active )
    , ( "activePast", Enc.list (List.map Enc.string vs.activePast) )
    , ( "activeFuture", Enc.list (List.map Enc.string vs.activeFuture) )
    , ( "descendants", Enc.list (List.map Enc.string vs.descendants) )
    , ( "editing", maybeToValue vs.editing Enc.string )
    ]


viewStateDecoder : Decoder ViewState
viewStateDecoder =
  Json.map5 ViewState
    (field "active" string)
    (field "activePast" (list string))
    (field "activeFuture" (list string))
    (field "descendants" (list string))
    (maybe (field "editing" string))




-- Status

statusToValue : Status -> Enc.Value
statusToValue status =
  case status of
    Clean head ->
      Enc.object
        [ ( "_id", "_local/status" |> Enc.string)
        , ( "status", "clean" |> Enc.string)
        , ( "head", head |> Enc.string)
        ]

    MergeConflict tree aSha bSha conflicts ->
      Enc.object
        [ ( "_id", "_local/status" |> Enc.string)
        , ( "status", "merge-conflict" |> Enc.string)
        , ( "tree", tree |> treeToValue)
        , ( "aSha", aSha |> Enc.string)
        , ( "bSha", bSha |> Enc.string)
        , ( "conflicts", Enc.list (List.map conflictToValue conflicts) )
        ]

    Bare ->
      Enc.object
        [ ( "_id", "_local/status" |> Enc.string)
        , ( "status", "bare" |> Enc.string)
        , ( "bare", Enc.bool True )
        ]


statusDecoder : Decoder Status
statusDecoder =
  let
    cleanDecoder =
      Json.map (\h -> Clean h)
        ( field "head" string)

    mergeConflictDecoder =
      Json.map4 (\t a b c -> MergeConflict t a b c)
        ( field "tree" treeDecoder)
        ( field "aSha" string )
        ( field "bSha" string )
        ( field "conflicts" (list conflictDecoder))

    bareDecoder =
      Json.map (\_ -> Bare)
        ( field "bare" bool )

  in
  oneOf
    [ cleanDecoder
    , mergeConflictDecoder
    , bareDecoder
    ]




-- Conflict

conflictToValue : Conflict -> Enc.Value
conflictToValue {id, opA, opB, selection, resolved} =
  Enc.object
    [ ("id", Enc.string id)
    , ("opA", opToValue opA)
    , ("opB", opToValue opB)
    , ("selection", selectionToValue selection)
    , ("resolved", Enc.bool resolved)
    ]


conflictDecoder : Decoder Conflict
conflictDecoder =
  Json.map5 Conflict
    ( field "id" string )
    ( field "opA" opDecoder )
    ( field "opB" opDecoder )
    ( field "selection" selectionDecoder )
    ( field "resolved" bool )




-- Op

opToValue : Op -> Enc.Value
opToValue op =
  case op of
    Mod str ->
      Enc.list ["mod" |> Enc.string, str |> Enc.string]

    _ ->
      Enc.null


opDecoder : Decoder Op
opDecoder =
  Json.map (\str -> Mod str)
    ( index 1 string )




-- Selection

selectionToValue : Selection -> Enc.Value
selectionToValue selection =
  case selection of
    Ours -> "ours" |> Enc.string
    Theirs -> "theirs" |> Enc.string
    Original -> "original" |> Enc.string
    Manual -> "manual" |> Enc.string


selectionDecoder : Decoder Selection
selectionDecoder =
  let
    fn s =
      case s of
        "ours" -> Ours
        "theirs" -> Theirs
        "original" -> Original
        "manual" -> Manual
        _ -> Manual
  in
  Json.map fn string




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
