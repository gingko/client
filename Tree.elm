module Tree exposing (..)

import Types exposing (..)




-- MODEL


type alias Tree =
  { uid : String
  , content : Content
  , prev : Maybe String
  , next : Maybe String
  , visible : Bool
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)




-- UPDATE





-- VIEW
