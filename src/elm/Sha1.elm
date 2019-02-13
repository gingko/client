module Sha1 exposing
    ( sha1
    , Diff(..), diff3Merge, timeJSON
    )

{-| A single function library, exposing sha1: String -> String.

SHA-1 function from [creationix/elm-git](https://github.com/creationix/elm-git).

@docs sha1

-}

import Json.Decode as Json
import SHA1


{-| Get the SHA-1 hash of a string.
-}
sha1 : String -> String
sha1 str =
    str |> SHA1.fromString |> SHA1.toHex


{-| Get Date.now() from JS as JSON
-}
timeJSON : () -> String
timeJSON a =
    -- TODO: Implement
    "0"


type Diff
    = DiffOk (List String)
    | DiffConflict ( List String, List String, List String )


{-| Get diff3Merge
-}
diff3Merge : List String -> List String -> List String -> List Diff
diff3Merge l o r =
    []
