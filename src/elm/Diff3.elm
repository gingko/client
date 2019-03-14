module Diff3 exposing (Diff(..), diff3Merge)

import Json.Decode as Json


type Diff
    = DiffOk (List String)
    | DiffConflict ( List String, List String, List String )


diff3Merge : List String -> List String -> List String -> List Diff
diff3Merge l o r =
    -- TODO: Was implemented with JS "native" code in elm 0.18
    []
