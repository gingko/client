module Doc.Data.Conflict exposing (Conflict, Op(..), Selection(..))

import Json.Decode as Dec exposing (Decoder)
import Json.Encode as Enc


type Selection
    = Original
    | Ours
    | Theirs
    | Manual


type alias Conflict =
    { id : String
    , opA : Op
    , opB : Op
    , selection : Selection
    , resolved : Bool
    }


type Op
    = Ins String String (List String) Int
    | Mod String (List String) String String
    | Del String (List String)
    | Mov String (List String) Int (List String) Int



-- CODERS & DECODERS


conflictToValue : Conflict -> Enc.Value
conflictToValue { id, opA, opB, selection, resolved } =
    Enc.object
        [ ( "id", Enc.string id )
        , ( "opA", opToValue opA )
        , ( "opB", opToValue opB )
        , ( "selection", selectionToValue selection )
        , ( "resolved", Enc.bool resolved )
        ]


conflictDecoder : Decoder Conflict
conflictDecoder =
    Dec.map5 Conflict
        (Dec.field "id" Dec.string)
        (Dec.field "opA" opDecoder)
        (Dec.field "opB" opDecoder)
        (Dec.field "selection" selectionDecoder)
        (Dec.field "resolved" Dec.bool)


opToValue : Op -> Enc.Value
opToValue op =
    case op of
        Mod id parents str orig ->
            Enc.list
                Enc.string
                ([ "mod", id, str, orig ] ++ parents)

        Del id parents ->
            Enc.list
                Enc.string
                ([ "del", id ] ++ parents)

        _ ->
            Enc.null


opDecoder : Decoder Op
opDecoder =
    let
        modDecoder =
            Dec.map4 (\id str orig parents -> Mod id parents str orig)
                (Dec.index 1 Dec.string)
                (Dec.index 2 Dec.string)
                (Dec.index 3 Dec.string)
                (Dec.index 4 (Dec.list Dec.string))

        delDecoder =
            Dec.map2 (\id parents -> Del id parents)
                (Dec.index 1 Dec.string)
                (Dec.index 2 (Dec.list Dec.string))
    in
    Dec.oneOf
        [ modDecoder
        , delDecoder
        ]



-- Selection


selectionToValue : Selection -> Enc.Value
selectionToValue selection =
    case selection of
        Ours ->
            "ours" |> Enc.string

        Theirs ->
            "theirs" |> Enc.string

        Original ->
            "original" |> Enc.string

        Manual ->
            "manual" |> Enc.string


selectionDecoder : Decoder Selection
selectionDecoder =
    let
        fn s =
            case s of
                "ours" ->
                    Ours

                "theirs" ->
                    Theirs

                "original" ->
                    Original

                "manual" ->
                    Manual

                _ ->
                    Manual
    in
    Dec.map fn Dec.string
