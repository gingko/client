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
    | Move Tree String Int
    | MoveWithin String Int
    | MoveLeft String
    | MoveRight String
    -- === History ===
    | Undo
    | Redo
    | Pull
    | Push
    | SetSelection String
    | Resolve String
    | CheckoutCommit String
    -- === Ports ===
    | Load Json.Value
    | MergeIn Json.Value
    | UpdateCommits (Json.Value, Maybe String)
    | AttemptCommit
    | HandleKey String




type alias Tree =
  { id : String
  , content : String
  , children : Children
  }

type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


type Op = Ins Tree (List String) Int | Mod String (List String) String | Del String (List String) | Mov String (List String) Int
type Selection = Original | Ours | Theirs | Manual
type alias Conflict =
  { id : String
  , opA : Op
  , opB : Op
  , selection : Selection
  , resolved : Bool
  }


type Status = Bare | Clean String | MergeConflict Tree String String (List Conflict)


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
