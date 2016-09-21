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


defaultContent : Content
defaultContent = { id = "0" , contentType = "text/markdown" , content = "defaultContent" }
