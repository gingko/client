module Coders exposing (collabStateDecoder, collabStateToValue, fontSettingsEncoder, lazyRecurse, maybeToValue, modeDecoder, modeToValue, treeDecoder, treeListDecoder, treeToJSON, treeToJSONrecurse, treeToMarkdown, treeToMarkdownRecurse, treeToMarkdownString, treeToValue, treesModelDecoder, tripleDecoder, tupleDecoder, tupleToValue)

import Doc.Fonts as Fonts
import Doc.TreeStructure as TreeStructure
import Json.Decode as Json exposing (..)
import Json.Encode as Enc
import Types exposing (..)



-- Tree


treeToValue : Tree -> Enc.Value
treeToValue tree =
    case tree.children of
        Children c ->
            Enc.object
                [ ( "id", Enc.string tree.id )
                , ( "content", Enc.string tree.content )
                , ( "children", Enc.list treeToValue c )
                ]


treesModelDecoder : Decoder TreeStructure.Model
treesModelDecoder =
    Json.map2 TreeStructure.Model
        treeDecoder
        (succeed [])


treeDecoder : Decoder Tree
treeDecoder =
    Json.map3 Tree
        (field "id" string)
        (field "content" string)
        (oneOf
            [ field
                "children"
                (list (lazyRecurse (\_ -> treeDecoder))
                    |> Json.map Children
                )
            , succeed (Children [])
            ]
        )


treeListDecoder : Decoder (List ( String, String ))
treeListDecoder =
    list (tupleDecoder string string)



-- ViewState


collabStateToValue : CollabState -> Enc.Value
collabStateToValue collabState =
    Enc.object
        [ ( "uid", Enc.string collabState.uid )
        , ( "mode", modeToValue collabState.mode )
        , ( "field", Enc.string collabState.field )
        ]


collabStateDecoder : Decoder CollabState
collabStateDecoder =
    Json.map3 CollabState
        (field "uid" string)
        (field "mode" modeDecoder)
        (field "field" string)



-- Mode


modeToValue : Mode -> Enc.Value
modeToValue mode =
    case mode of
        CollabActive id ->
            tupleToValue Enc.string ( "CollabActive", id )

        CollabEditing id ->
            tupleToValue Enc.string ( "CollabEditing", id )


modeDecoder : Decoder Mode
modeDecoder =
    let
        modeHelp : ( String, String ) -> Decoder Mode
        modeHelp ( tag, idIn ) =
            case ( tag, idIn ) of
                ( "CollabActive", id ) ->
                    succeed (CollabActive id)

                ( "CollabEditing", id ) ->
                    succeed (CollabEditing id)

                _ ->
                    fail <| "Failed mode decoder"
    in
    tupleDecoder string string
        |> andThen modeHelp


treeToJSON : Tree -> Enc.Value
treeToJSON tree =
    case tree.children of
        Children c ->
            Enc.list treeToJSONrecurse c


treeToJSONrecurse : Tree -> Enc.Value
treeToJSONrecurse tree =
    case tree.children of
        Children c ->
            Enc.object
                [ ( "content", Enc.string tree.content )
                , ( "children", Enc.list treeToJSONrecurse c )
                ]


treeToMarkdown : Bool -> Tree -> Enc.Value
treeToMarkdown withRoot tree =
    tree
        |> treeToMarkdownString withRoot
        |> Enc.string


treeToMarkdownString : Bool -> Tree -> String
treeToMarkdownString withRoot tree =
    let
        contentList =
            case tree.children of
                Children c ->
                    List.map treeToMarkdownRecurse c
    in
    if withRoot then
        tree.content
            :: contentList
            |> String.join "\n\n"

    else
        contentList
            |> String.join "\n\n"


treeToMarkdownRecurse : Tree -> String
treeToMarkdownRecurse tree =
    case tree.children of
        Children c ->
            [ tree.content ]
                ++ List.map treeToMarkdownRecurse c
                |> String.join "\n\n"



-- FONT SETTINGS


fontSettingsEncoder : Fonts.Settings -> Enc.Value
fontSettingsEncoder { heading, content, monospace } =
    Enc.list Enc.string [ heading, content, monospace ]



-- HELPERS


lazyRecurse : (() -> Decoder a) -> Decoder a
lazyRecurse thunk =
    let
        toResult =
            \js -> decodeValue (thunk ()) js
    in
    andThen
        (\a ->
            case toResult a of
                Ok b ->
                    succeed b

                Err err ->
                    fail (errorToString err)
        )
        value


maybeToValue : (a -> Enc.Value) -> Maybe a -> Enc.Value
maybeToValue encoder mb =
    case mb of
        Nothing ->
            Enc.null

        Just v ->
            encoder v


tupleToValue : (a -> Enc.Value) -> ( a, a ) -> Enc.Value
tupleToValue encoder ( aVal, bVal ) =
    Enc.list encoder [ aVal, bVal ]


tupleDecoder : Decoder a -> Decoder b -> Decoder ( a, b )
tupleDecoder a b =
    index 0 a
        |> andThen
            (\aVal ->
                index 1 b
                    |> andThen (\bVal -> succeed ( aVal, bVal ))
            )


tripleDecoder : Decoder a -> Decoder b -> Decoder c -> Decoder ( a, b, c )
tripleDecoder a b c =
    index 0 a
        |> andThen
            (\aVal ->
                index 1 b
                    |> andThen
                        (\bVal ->
                            index 2 c
                                |> andThen (\cVal -> succeed ( aVal, bVal, cVal ))
                        )
            )
