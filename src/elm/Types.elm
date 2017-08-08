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
    | UpdateContent (String, String) -- |> Save
    | UpdateCardError String
    | DeleteCard String -- |> Activate |> Save
    | CancelCard
    -- === Card Insertion  ===
    | Insert String Int -- |> OpenCard |> Activate
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | Move Tree String Int -- |> Activate |> Save
    | MoveWithin String Int
    | MoveLeft String
    | MoveRight String
    -- === History ===
    | Undo
    | Redo
    | Pull
    | Push
    | SetSelection String Selection String
    | Resolve String -- |> Save
    | CheckoutCommit String -- |> UpdateCommits
    -- === Files ===
    | IntentNew
    | IntentSave
    | IntentOpen
    -- === Ports ===
    | ExternalMessage (String, String)
    | Load (Maybe String, Json.Value) -- |> UpdateCommits
    | MergeIn Json.Value -- |> [Save] |> UpdateCommits
    | ImportJson Json.Value
    | SetHeadRev String
    | UpdateCommits (Json.Value, Maybe String)
    | Save -- |> UpdateCommits |> [Push]
    | SendCollabState CollabState
    | RecvCollabState Json.Value
    | CollaboratorDisconnected String
    | HandleKey String




type alias Tree =
  { id : String
  , content : String
  , children : Children
  }

type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)


type Op = Ins String String (List String) Int | Mod String (List String) String String | Del String (List String) | Mov String (List String) Int (List String) Int
type Selection = Original | Ours | Theirs | Manual
type alias Conflict =
  { id : String
  , opA : Op
  , opB : Op
  , selection : Selection
  , resolved : Bool
  }


type Status = Bare | Clean String | MergeConflict Tree String String (List Conflict)


type Mode = Active String | Editing String


type alias CollabState =
  { uid : String
  , mode : Mode
  , field : String
  }


type alias ViewState =
  { active : String
  , activePast : List String
  , activeFuture : List String
  , descendants : List String
  , editing : Maybe String
  , collaborators : List CollabState
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  , collaborators : List CollabState
  }
