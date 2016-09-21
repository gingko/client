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

type alias ViewState =
  { active : String
  , editing : Maybe String
  , field : String
  }

type alias Tree =
  { uid : String
  , content : Content
  , parentId : Maybe String
  , prev : Maybe String
  , next : Maybe String
  , visible : Bool
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


defaultContent : Content
defaultContent = { id = "0" , contentType = "text/markdown" , content = "defaultContent" }
