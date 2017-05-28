module Sha1 exposing (sha1, timestamp, timeJSON, diff3Merge, Diff(DiffOk, DiffConflict))

{-| A single function library, exposing sha1: String -> String.

SHA-1 function from [creationix/elm-git](https://github.com/creationix/elm-git).

@docs sha1
-}

import Json.Decode as Json
import Native.Sha1


{-| Get the SHA-1 hash of a string.
-}
sha1: String -> String
sha1 str = Native.Sha1.sha1 str


{-| Get Date.now() from JS as Int
-}
timestamp : () -> Int
timestamp a = Native.Sha1.timestamp a


{-| Get Date.now() from JS as JSON
-}
timeJSON : () -> String
timeJSON a = Native.Sha1.timeJSON a


type Diff = DiffOk (List String) | DiffConflict (List String, List String, List String)

{-| Get diff3Merge
-}
diff3Merge : List String -> List String -> List String -> List Diff
diff3Merge l o r =
  Native.Sha1.diff3Merge l o r


