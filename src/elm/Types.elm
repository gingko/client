module Types exposing (..)

import Json.Decode as Json
import Html5.DragDrop as DragDrop
import Trees exposing (Tree)




type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    -- === Card Editing  ===
    | OpenCard String String
    | UpdateContent (String, String)
    | DeleteCard String
    | CancelCard
    -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | DragDropMsg (DragDrop.Msg String DropId)
    -- === History ===
    | Undo
    | Redo
    | Pull
    | SetSelection String Selection String
    | Resolve String
    | CheckoutCommit String
    -- === Ports ===
    | Outside InfoForElm
    | ExternalMessage (String, String)
    | Load (Maybe String, Json.Value)
    | MergeIn Json.Value
    | ImportJson Json.Value
    | SetHeadRev String
    | RecvCollabState Json.Value
    | CollaboratorDisconnected String
    | HandleKey String


type InfoForOutside
    = Alert String
    | Push
    --| Pull
    | Changed
    | New (Maybe String)
    | Save (Maybe String)
    | SaveAs
    | ExportJSON Tree
    | ExportTXT Tree
    | SocketSend CollabState


type InfoForElm
    = Reset
    --| Load String Json.Value
    | Saved
    | DoExportJSON
    | DoExportTXT
    | Keyboard String


type alias OutsideData =
  { tag : String, data: Json.Value }



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


type DropId = Above String | Below String | Into String


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
  , dragModel : DragDrop.Model String DropId
  , draggedTree : Maybe (Tree, String, Int)
  , collaborators : List CollabState
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  , dragModel : DragDrop.Model String DropId
  , collaborators : List CollabState
  }
