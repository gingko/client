module DataTests exposing (..)

import Doc.Data as Data exposing (Card_tests_only, localSave, model_tests_only, toSave_tests_only)
import Expect exposing (Expectation)
import Json.Encode as Enc exposing (list, object, string)
import Test exposing (..)
import Types exposing (CardTreeOp(..))
import UpdatedAt


suite : Test
suite =
    describe "localSave"
        [ test "CTIns to empty model" <|
            \_ ->
                localSave "treeId" (CTIns "someid" "new content" Nothing 0) Data.emptyCardBased
                    |> expectEqualJSON
                        (toSave
                            { toAdd =
                                [ card "someid" "treeId" "new content" Nothing 0.0 False False () ]
                            , toMarkSynced = []
                            , toMarkDeleted = []
                            , toRemove = []
                            }
                        )
        , test "CTIns to model with existing card" <|
            \_ ->
                let
                    data =
                        d
                            [ card "otherId"
                                "treeId"
                                ""
                                Nothing
                                0.0
                                False
                                True
                                (u 12345 0 "afe223")
                            ]
                            Nothing
                in
                localSave "treeId" (CTIns "someid" "new content" Nothing 1) data
                    |> expectEqualJSON
                        (toSave
                            { toAdd =
                                [ card "someid" "treeId" "new content" Nothing 1 False False () ]
                            , toMarkSynced = []
                            , toMarkDeleted = []
                            , toRemove = []
                            }
                        )
        , test "CTUpd existing card" <|
            \_ ->
                let
                    data =
                        d
                            [ card "someid"
                                "treeId"
                                "old content"
                                Nothing
                                0.0
                                False
                                True
                                (u 12345 0 "afe223")
                            ]
                            Nothing
                in
                localSave "treeId" (CTUpd "someid" "new content") data
                    |> expectEqualJSON
                        (toSave
                            { toAdd =
                                [ card "someid"
                                    "treeId"
                                    "new content"
                                    Nothing
                                    0.0
                                    False
                                    False
                                    ()
                                ]
                            , toMarkSynced = []
                            , toMarkDeleted = []
                            , toRemove = []
                            }
                        )
        , test "CTUpd non-existing card" <|
            \_ ->
                let
                    data =
                        d
                            [ card "otherId"
                                "treeId"
                                ""
                                Nothing
                                0.0
                                False
                                True
                                (u 12345 0 "afe223")
                            ]
                            Nothing
                in
                localSave "treeId" (CTUpd "someid" "new content") data
                    |> expectEqualJSON
                        (toSave
                            { toAdd =
                                [ card "someid"
                                    "treeId"
                                    "new content"
                                    Nothing
                                    0.0
                                    False
                                    False
                                    ()
                                ]
                            , toMarkSynced = []
                            , toMarkDeleted = []
                            , toRemove = []
                            }
                        )
        ]



-- HELPERS


expectEqualJSON : Enc.Value -> Enc.Value -> Expectation
expectEqualJSON a b =
    Expect.equal (Enc.encode 0 a) (Enc.encode 0 b)


card =
    Card_tests_only


toSave =
    toSave_tests_only


d =
    model_tests_only


u a b c =
    UpdatedAt.fromParts a b c
