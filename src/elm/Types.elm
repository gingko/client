module Types exposing (..)

import Dict exposing (Dict)
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
    | TryMerge
    | LoadCommit String
    -- === Ports ===
    | ChangeIn Json.Value
    | ObjectsIn Json.Value
    | UpdateCommits (Json.Value, String)
    | AttemptCommit
    | HandleKey String



-- TREES

type alias Tree =
  { id : String
  , content : String
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)




-- COMMUNICATION

type alias ResOk =
  { id : String
  , ok : Bool
  , rev : String
  }

type alias ResErr =
  { status : Int
  , name : String
  , message : String
  , error : Bool
  }




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
