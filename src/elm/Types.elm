module Types exposing (..)

import Json.Decode as Json




type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    | GoLeft String
    | GoDown String
    | GoUp String
    | GoRight String
    -- === Card Editing  ===
    | OpenCard String String
    | GetContentToSave String
    | UpdateContent (String, String)
    | UpdateCardError String
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
    | Insert String Int
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | Move String String Int
    | MoveWithin String Int
    | MoveLeft String
    | MoveRight String
    -- === Ports ===
    | NodesIn Json.Value
    | ChangeIn Json.Value
    | ConflictsIn Json.Value
    | AttemptSave
    | SaveResponses (List Response)
    | HandleKey String



-- OBJECTS


type alias Tree =
  { id : String
  , content : String
  , children : Children
  , rev : Maybe String
  }


type alias TreeNode =
  { content : String
  , children : List (String, Bool)
  , rev : Maybe String
  , deletedWith : Maybe (List String)
  , deleted_ : Bool
  }


type alias Response =
  { id : String
  , rev : String
  , ok : Bool
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)




-- TRANSIENTS

type alias ViewState =
  { active : String
  , activePast : List String
  , activeFuture : List String
  , descendants : List String
  , editing : Maybe String
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  }
