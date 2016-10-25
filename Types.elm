module Types exposing (..)

import Json.Decode as Json



type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    | ActivatePast
    | ActivateFuture
    | GoLeft String
    | GoDown String
    | GoUp String
    | GoRight String
    -- === Card Editing  ===
    | OpenCard String String
    | UpdateField String
    | UpdateCard String String
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
    | Insert (Maybe String) (Maybe String) (Maybe String)
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    -- === External Inputs ===
    | OpIn Json.Value
    | ExternalCommand (String, String)
    | HandleKey String



-- OBJECTS

type alias Content =
  { id : String
  , contentType : String
  , content : String
  }

type alias Node =
  { id : String
  , contentId : String
  , parentId: Maybe String
  , position: Int
  }

type Op
  = Ins String Content (Maybe String) Int (Maybe String)
  | Del String




-- TRANSIENTS

type alias ViewState =
  { active : String
  , activePast : List String
  , activeFuture : List String
  , descendants : List String
  , editing : Maybe String
  , field : String
  }


type alias Tree =
  { id : String
  , content : Content
  , parentId : Maybe String
  , position : Int
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)




-- DEFAULTS

defaultContent : Content
defaultContent =
  { id = -- sha1(contentType+"\n"+content)
      "5960f096212e449474d2eb1f8f4e33495d0a53aa" 
  , contentType = "text/markdown" 
  , content = "fromDefault"
  }

defaultNode : Node
defaultNode =
  { id = -- sha1(contentId+"\n"+childrenIds.join("\n")
      "1331dfaaa7dc267a902e4a9aa0e9d97130fabc4c" 
  , contentId = defaultContent.id
  , parentId = Nothing
  , position = 0
  }

