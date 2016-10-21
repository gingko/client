module Types exposing (..)

import Json.Decode as Json



type Msg
    = NoOp
    -- === Commits ===
    | CommitAll Int
    | CommitChanges Int Int
    | CheckoutCommit String
    -- === Operations ===
    | CheckOp String Bool
    | DeleteOp String
    | Undo
    | Redo
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
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | MoveUp String
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
  , childrenIds : List String
  }

type alias Commit =
  { id : String
  , rootNode : String
  , timestamp : Int
  , authors : List String
  , committer : String
  , parents: List String
  , message : String
  }

type Op
  = Ins String (Maybe String) (Maybe String) (Maybe String) Int
  | Upd String String String Int
  | Del String String Int
  | Copy String String (Maybe String) (Maybe String) (Maybe String) Int

type alias Objects =
  { contents : List Content
  , nodes : List Node
  , commits : List Commit
  , operations : List Op
  }




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
  , childrenIds = []
  }

defaultCommit : Commit
defaultCommit =
  { id = -- sha1(rootNode+"\n"+parents.join("\n")+authors.join("\n")+commiter+"\n"+timestamp+"\n"+message)
      "bc5ff95381ff8577663e45455b14cd09d7e126c1"
  , rootNode = defaultNode.id
  , timestamp = 1474906969610
  , authors = ["Gingko"]
  , committer = "Gingko"
  , parents= []
  , message = "Initial commit."
  }

defaultObjects : Objects
defaultObjects =
  { contents = [defaultContent]
  , nodes = [defaultNode]
  , commits = [defaultCommit]
  , operations = []
  }
