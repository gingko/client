module ParserTests exposing (..)

import Coders exposing (normalizeInput, treesParser)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, list, string)
import Parser
import Test exposing (..)
import Types exposing (..)


suite : Test
suite =
    describe "Regex normalizing then parse"
        [ describe "Regex normalizing"
            [ test "replace open & close tags" <|
                \_ ->
                    "<gingko-card id=\"1234\">content</gingko-card>"
                        |> normalizeInput
                        |> Expect.equal "%!#<gingko-card|1234>content%!#<gingko-card/>"
            , test "normalize nested and multi-line" <|
                \_ ->
                    "<gingko-card id=\"abc\">\n\n# Content\n\n< gingko-card    id=\"xyz\" >\n\nchild content\n\n</ gingko-card>\n</gingko-card>"
                        |> normalizeInput
                        |> Expect.equal "%!#<gingko-card|abc>\n\n# Content\n\n%!#<gingko-card|xyz>\n\nchild content\n\n%!#<gingko-card/>%!#<gingko-card/>"
            , test "multiple children" <|
                \_ ->
                    """<gingko-card id="0"><gingko-card id="first">content</gingko-card><gingko-card id="second">2nd</gingko-card></gingko-card>"""
                        |> normalizeInput
                        |> Expect.equal "%!#<gingko-card|0>%!#<gingko-card|first>content%!#<gingko-card/>%!#<gingko-card|second>2nd%!#<gingko-card/>%!#<gingko-card/>"
            , test "multiple trees" <|
                \_ ->
                    """<gingko-card id="first">content</gingko-card><gingko-card id="second">2nd</gingko-card>"""
                        |> normalizeInput
                        |> Expect.equal "%!#<gingko-card|first>content%!#<gingko-card/>%!#<gingko-card|second>2nd%!#<gingko-card/>"
            ]
        , describe "Parsing normalized string"
            [ test "Empty Tree" <|
                \_ ->
                    """<gingko-card id="root-id1"></gingko-card>"""
                        |> expectTrees [ Tree "root-id1" "" noChildren ]
            , test "tree with content and no children" <|
                \_ ->
                    "<gingko-card id=\"root-id2\">\n\n# A heading here\n\n</gingko-card>"
                        |> expectTrees [ Tree "root-id2" "# A heading here" (Children []) ]
            , test "tree with content and one child" <|
                \_ ->
                    "<gingko-card id=\"root-id3\">\n\n# A heading here\n\n<gingko-card id=\"child-card\">\n\nSomething here\n\n</gingko-card></gingko-card>\"\n"
                        |> expectTrees [ Tree "root-id3" "# A heading here" (Children [ Tree "child-card" "Something here" (Children []) ]) ]
            , test "multiple children" <|
                \_ ->
                    """<gingko-card id="0"><gingko-card id="first">content</gingko-card><gingko-card id="second">2nd</gingko-card></gingko-card>"""
                        |> expectTrees
                            [ Tree "0" "" (Children [ Tree "first" "content" noChildren, Tree "second" "2nd" noChildren ]) ]
            , test "multiple trees" <|
                \_ ->
                    """<gingko-card id="first">content</gingko-card><gingko-card id="second">2nd</gingko-card>"""
                        |> expectTrees
                            [ Tree "first" "content" noChildren, Tree "second" "2nd" noChildren ]
            , test "depth" <|
                \_ ->
                    "<gingko-card id=\"1\">\n\n# Root card\n\n<gingko-card id=\"node-912581817\">\n\n## A\n\n</gingko-card>\n<gingko-card id=\"node-84350036\">\n\n## B\n\n<gingko-card id=\"node-475880497\">\n\nB 1\n\nWith some new lines.\nHere\n\nAnd here\n\n</gingko-card>\n</gingko-card>\n<gingko-card id=\"node-119088364\">\n\n## C\n\n</gingko-card>\n</gingko-card>"
                        |> expectTrees
                            [ Tree "1"
                                "# Root card"
                                (Children
                                    [ Tree "node-912581817" "## A" noChildren
                                    , Tree "node-84350036" "## B" (Children [ Tree "node-475880497" "B 1\n\nWith some new lines.\nHere\n\nAnd here" noChildren ])
                                    , Tree "node-119088364" "## C" noChildren
                                    ]
                                )
                            ]
            ]
        , describe "fuzzy parsing tests"
            [ fuzz3 string string string "root, child, grandchild with fuzzy content" <|
                \s1 s2 s3 ->
                    "<gingko-card id=\"0\">"
                        ++ s1
                        ++ "<gingko-card id=\"a\">"
                        ++ s2
                        ++ "<gingko-card id=\"a1\">"
                        ++ s3
                        ++ "</gingko-card></gingko-card></gingko-card>"
                        |> expectTrees [ Tree "0" s1 (Children [ Tree "a" s2 (Children [ Tree "a1" s3 noChildren ]) ]) ]
            ]
        ]



-- HELPERS


expectTrees : List Tree -> String -> Expectation
expectTrees expected input =
    input
        |> normalizeInput
        |> Parser.run treesParser
        |> Expect.equal (Ok expected)


noChildren =
    Children []
