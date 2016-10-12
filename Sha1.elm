module Sha1 exposing (sha1, timestamp)

{-| A single function library, exposing sha1: String -> String.

SHA-1 function from [creationix/elm-git](https://github.com/creationix/elm-git).

@docs sha1
-}

import Native.Sha1


{-| Get the SHA-1 hash of a string.
-}
sha1: String -> String
sha1 str = Native.Sha1.sha1 str


{-| Get Date.now() from JS
-}
timestamp : () -> Int
timestamp a = Native.Sha1.timestamp a


