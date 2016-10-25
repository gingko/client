module TreeUtils exposing (..)

import String
import List.Extra as ListExtra
import Types exposing (..)
import Sha1



-- TREE MODIFICATIONS --




-- TREE AND NODE TRANSFORMATIONS




-- ACCESSORS




-- HELPERS

(?) : Maybe a -> a -> a
(?) maybe default =
  Maybe.withDefault default maybe

infixr 9 ?

newLine : String
newLine =
  String.fromList ['\n']

