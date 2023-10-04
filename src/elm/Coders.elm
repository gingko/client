module Coders exposing (collabStateDecoder, collabStateEncoder, fontSettingsEncoder, lazyRecurse, maybeToValue, modeDecoder, normalizeAndParse, normalizeInput, sortByDecoder, sortByEncoder, treeDecoder, treeOrString, treeToJSON, treeToJSONrecurse, treeToMarkdownOutline, treeToMarkdownRecurse, treeToMarkdownString, treeToOPML, treeToValue, treesParser, tupleDecoder, tupleToValue)

import Doc.Fonts as Fonts
import Json.Decode as Json exposing (..)
import Json.Encode as Enc
import Parser exposing ((|.), (|=), DeadEnd, Parser, Step(..), Trailing(..), chompUntil, getChompedString, keyword, loop, spaces, symbol)
import Regex
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


treeOrString : Decoder Tree
treeOrString =
    Json.oneOf
        [ treeDecoder
        , Json.map (\str -> Tree "0" str (Children [])) Json.string
        ]



-- ViewState


collabStateDecoder : Decoder CollabState
collabStateDecoder =
    Json.map3 CollabState
        (field "uid" string)
        (field "mode" modeDecoder)
        (field "field" string)



-- Mode


collabStateEncoder : CollabStateMode -> Enc.Value
collabStateEncoder mode =
    case mode of
        CollabActive id ->
            tupleToValue Enc.string ( "CollabActive", id )

        CollabEditing id ->
            tupleToValue Enc.string ( "CollabEditing", id )


modeDecoder : Decoder CollabStateMode
modeDecoder =
    let
        modeHelp : ( String, String ) -> Decoder CollabStateMode
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



-- JSON


treeToJSON : Bool -> Tree -> Enc.Value
treeToJSON withRoot tree =
    if withRoot then
        Enc.list treeToJSONrecurse [ tree ]

    else
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


attrEncode s =
    s
        |> String.replace "&" "&amp;"
        |> String.replace "\"" "&quot;"
        |> String.replace "'" "&apos;"
        |> String.replace "<" "&lt;"



-- OPML


treeToOPML : String -> Tree -> String
treeToOPML docname tree =
    "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n<opml version=\"2.0\">\n<head><title>"
        ++ (attrEncode docname |> String.replace ">" "&gt;")
        ++ "</title></head>\n<body>"
        ++ treeToOPMLBody tree
        ++ "</body></opml>"


treeToOPMLBody : Tree -> String
treeToOPMLBody tree =
    case tree.children of
        Children c ->
            "<outline text=\"" ++ attrEncode tree.content ++ "\">" ++ (List.map treeToOPMLBody c |> String.join "\n") ++ "</outline>\n"



-- Structured Markdown


treeToMarkdownOutline : Bool -> Tree -> String
treeToMarkdownOutline withRoot tree =
    if withRoot then
        treeToMarkdownOutlineRecurse tree

    else
        case tree.children of
            Children c ->
                List.map treeToMarkdownOutlineRecurse c |> String.join "\n"


treeToMarkdownOutlineRecurse : Tree -> String
treeToMarkdownOutlineRecurse tree =
    case tree.children of
        Children c ->
            "<gingko-card id=\""
                ++ tree.id
                ++ "\">\n\n"
                ++ tree.content
                ++ "\n\n"
                ++ (List.map treeToMarkdownOutlineRecurse c ++ [ "" ] |> String.join "\n")
                ++ "</gingko-card>"



-- NORMALIZER


normalizeInput : String -> String
normalizeInput str =
    str
        |> normalizeOpenTags
        |> normalizeCloseTags


normalizeOpenTags : String -> String
normalizeOpenTags str =
    let
        regex =
            Regex.fromString "<\\s*gingko-card\\s{1,}id=\"([^\"]*)\"\\s*>"
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex
        (\m ->
            case m.submatches of
                (Just idMatch) :: _ ->
                    "%!#<gingko-card|" ++ idMatch ++ ">"

                _ ->
                    m.match
        )
        str


normalizeCloseTags : String -> String
normalizeCloseTags str =
    let
        regex =
            Regex.fromString "<\\/\\s*gingko-card\\s*>\\s*"
                |> Maybe.withDefault Regex.never
    in
    Regex.replace regex
        (\_ -> "%!#<gingko-card/>")
        str



-- PARSER


normalizeAndParse : String -> Result (List DeadEnd) (List Tree)
normalizeAndParse input =
    input
        |> normalizeInput
        |> Parser.run treesParser


treesParser : Parser (List Tree)
treesParser =
    loop [] treeListHelper


treeListHelper : List Tree -> Parser (Step (List Tree) (List Tree))
treeListHelper revTrees =
    Parser.oneOf
        [ Parser.succeed (\tree -> Loop (tree :: revTrees))
            |= parseTree
        , Parser.succeed (Done (List.reverse revTrees))
        ]


parseTree : Parser Tree
parseTree =
    Parser.succeed (\id str ch -> Tree id str (Children ch))
        |= openTagParser
        |= contentParser
        |= treesParser
        |. closeTagParser


openTagParser : Parser String
openTagParser =
    Parser.succeed identity
        |. symbol "%!#<gingko-card|"
        |= getChompedString (chompUntil ">")
        |. symbol ">"


contentParser : Parser String
contentParser =
    getChompedString
        (chompUntil "%!#<gingko-card")
        |> Parser.map
            (\s ->
                case ( String.startsWith "\n\n" s, String.endsWith "\n\n" s, String.length s >= 4 ) of
                    ( True, True, True ) ->
                        s |> String.dropLeft 2 |> String.dropRight 2

                    _ ->
                        s
            )


closeTagParser : Parser ()
closeTagParser =
    Parser.oneOf
        [ symbol "%!#<gingko-card/>"
        , Parser.end
        ]



-- Markdown


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



-- SortBy


sortByDecoder : Decoder SortBy
sortByDecoder =
    let
        get id =
            case id of
                "Alphabetical" ->
                    succeed Alphabetical

                "ModifiedAt" ->
                    succeed ModifiedAt

                "CreatedAt" ->
                    succeed CreatedAt

                _ ->
                    fail ("unknown value for SortBy: " ++ id)
    in
    string |> andThen get


sortByEncoder : SortBy -> Enc.Value
sortByEncoder sortBy =
    case sortBy of
        Alphabetical ->
            Enc.string "Alphabetical"

        ModifiedAt ->
            Enc.string "ModifiedAt"

        CreatedAt ->
            Enc.string "CreatedAt"



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
