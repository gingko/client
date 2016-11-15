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
    | SaveCard
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
    -- === Ports ===
    | SaveTemp
    | Confirm String String String
    | DataIn Json.Value
    | ExternalCommand (String, String)
    | HandleKey String



-- OBJECTS




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
  , content : String
  , children : Children
  }


type Children = Children (List Tree)
type alias Group = List Tree
type alias Column = List (List Tree)
