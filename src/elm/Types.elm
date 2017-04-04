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
    | AttemptUpdateCard String
    | UpdateCard (String, String)
    | UpdateCardError String
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
    | Insert Tree String Int
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | Move Tree String Int
    | MoveUp String
    | MoveDown String
    | MoveLeft String
    | MoveRight String
    -- === History ===
    | Undo
    | Redo
    | AddToUndo Tree
    -- === File Handling ===
    | AttemptNew
    | AttemptOpen
    | AttemptImport
    | AttemptSave
    | AttemptSaveAs
    | AttemptSaveAndClose
    | SaveSuccess String
    | SaveError
    | SaveTemp
    -- === Ports ===
    | Confirm String String String
    | DataIn Json.Value
    | NodesIn Json.Value
    | ChangeIn (String, Json.Value)
    | ExternalCommand (String, String)
    | HandleKey String



-- OBJECTS


type alias Tree =
  { id : String
  , content : String
  , children : Children
  , rev : Maybe String
  , deleted : Bool
  }


type alias TreeNode =
  { content : String
  , children : List String
  , rev : Maybe String
  , deleted : Bool
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
