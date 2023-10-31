module DataTests exposing (..)

import Doc.Data as Data exposing (Card_tests_only, localSave, toSave_tests_only)
import Expect exposing (Expectation)
import Json.Encode as Enc exposing (list, object, string)
import Test exposing (..)
import Types exposing (CardTreeOp(..))


suite : Test
suite =
    describe "Data"
        [ test "localSave" <|
            \_ ->
                localSave "treeId" (CTIns "someid" "new content" Nothing 0) Data.emptyCardBased
                    |> expectEqualJSON
                        (toSave_tests_only
                            { toAdd =
                                [ Card_tests_only
                                    "someid"
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
