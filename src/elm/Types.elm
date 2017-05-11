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
    | Fetch
    | Push
    | TryMerge
    | SetSelection String
    | CheckoutCommit String
    -- === Ports ===
    | ObjectsIn Json.Value
    | UpdateCommits (Json.Value, String)
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


type Op = Mod String | Del | Mov String Int
type Selection = Original | Ours | Theirs | Manual
type alias Conflict =
  { id : String
  , opA : Op
  , opB : Op
  , selection : Selection
  , resolved : Bool
  }


type Status = Bare | Clean String | MergeConflict String String (List Conflict)


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
