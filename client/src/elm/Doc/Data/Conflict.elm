module Doc.Data.Conflict exposing (Conflict, Op(..), Selection(..), conflictWithSha, opString)

import Doc.TreeUtils exposing (sha1)
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


conflictWithSha : Conflict -> Conflict
conflictWithSha { opA, opB, selection, resolved } =
    Conflict
        (String.join "\n"
            [ opString opA
            , opString opB
            , selString selection
            , if resolved then
                "True"

              else
                "False"
            ]
            |> sha1
        )
        opA
        opB
        selection
        resolved


opString : Op -> String
opString op =
    case op of
        Ins st1 st2 strs int ->
            String.join "\n" ([ "Ins", st1, st2 ] ++ strs ++ [ String.fromInt int ])

        Mod st1 strs st2 st3 ->
            String.join "\n" ([ "Mod", st1 ] ++ strs ++ [ st2, st3 ])

        Del st1 strs ->
            String.join "\n" ([ "Del", st1 ] ++ strs)

        Mov st1 strs int strs2 int2 ->
            String.join "\n" ([ "Mov", st1 ] ++ strs ++ [ String.fromInt int ] ++ strs2 ++ [ String.fromInt int2 ])


selString : Selection -> String
selString selection =
    case selection of
        Ours ->
            "ours"

        Theirs ->
            "theirs"

        Original ->
            "original"

        Manual ->
            "manual"



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
    selString selection |> Enc.string


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
