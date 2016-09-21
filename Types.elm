module Types exposing (..)


type alias Content =
  { id : String
  , contentType : String
  , content : String
  }

type alias Node =
  { id : String
  , contentId : String
  , childrenIds : List String
  }

type alias Data =
  { contents : List Content
  , nodes : List Node
  , rootId : String
  }
