module Types exposing (..)

import Json.Decode as Json
import Html5.DragDrop as DragDrop


type Msg
    = NoOp
    -- === Card Activation ===
    | Activate String
    -- === Card Editing  ===
    | OpenCard String String
    | DeleteCard String
    -- === Card Insertion  ===
    | InsertAbove String
    | InsertBelow String
    | InsertChild String
    -- === Card Moving  ===
    | DragDropMsg (DragDrop.Msg String DropId)
    -- === History ===
    | Undo
    | Redo
    | Sync
    | SetSelection String Selection String
    | Resolve String
    -- === Help ===
    | VideoModal Bool
    | ShortcutTrayToggle
    -- === Ports ===
    | Port IncomingMsg
    | LogErr String


type OutgoingMsg
    = Alert String
    | ActivateCards (String, Int, List (List String))
    | GetText String
    | TextSurround String String
    | ConfirmCancel String String
    | ColumnNumberChange Int
    | New (Maybe String)
    | Open (Maybe String)
    | Save String
    | SaveAs
    | ExportJSON Tree
    | ExportTXT Bool Tree
    | ExportTXTColumn Int Tree
    | Push
    | Pull
    | SaveObjects (Json.Value, Json.Value)
    | SaveLocal Tree
    | UpdateCommits (Json.Value, Maybe String)
    | SetSaved String
    | SetChanged
    | SetVideoModal Bool
    | SetShortcutTray Bool
    | SocketSend CollabState
    | ConsoleLogRequested String


type IncomingMsg
    = UpdateContent (String, String)
    | CancelCardConfirmed
    | Reset
    | Load (String, Json.Value, String)
    | Merge Json.Value
    | ImportJSON Json.Value
    | CheckoutCommit String
    | SetHeadRev String
    | Changed
    | Saved String
    | RecvCollabState CollabState
    | CollaboratorDisconnected String
    | DoExportJSON
    | DoExportTXT
    | DoExportTXTCurrent
    | DoExportTXTColumn Int
    | ViewVideos
    | Keyboard String


type alias OutsideData =
  { tag : String, data: Json.Value }


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


type alias WordCount =
  { card : Int
  , subtree : Int
  , group : Int
  , column : Int
  , document : Int
  }
