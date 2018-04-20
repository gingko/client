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
    -- === Dialogs, Menus, Window State ===
    = Alert String
    | OpenDialog (Maybe String)
    | ImportDialog (Maybe String)
    | ConfirmClose String (Maybe String) ( Json.Value, Json.Value )
    | ConfirmCancelCard String String
    | ColumnNumberChange Int
    | ChangeTitle ( Maybe String ) Bool
    | Exit
    -- === Database ===
    | SaveToDB ( Json.Value, Json.Value )
    | SaveLocal Tree
    | ClearDB
    | Push
    | Pull
    -- === File System ===
    | Save ( Maybe String )
    | SaveAs ( Maybe String )
    | ExportJSON Tree ( Maybe String )
    | ExportTXT Bool Tree ( Maybe String )
    | ExportTXTColumn Int Tree ( Maybe String )
    -- === DOM ===
    | ActivateCards ( String, Int, List (List String), Maybe String )
    | TextSurround String String
    -- === UI ===
    | UpdateCommits ( Json.Value, Maybe String )
    | SetVideoModal Bool
    | SetShortcutTray Bool
    -- === Misc ===
    | SocketSend CollabState
    | ConsoleLogRequested String


type IncomingMsg
    -- === Dialogs, Menus, Window State ===
    = IntentNew
    | IntentOpen
    | IntentImport
    | IntentSave
    | IntentSaveAs
    | IntentExport ExportSettings
    | IntentExit
    | CancelCardConfirmed
    -- === Database ===
    | New
    | Open ( String, Json.Value, String )
    | SetHeadRev String
    | Merge Json.Value
    | ImportJSON Json.Value
    -- === File System ===
    | FileState ( Maybe String ) Bool
    -- === DOM ===
    | FieldChanged String
    -- === UI ===
    | CheckoutCommit String
    | ViewVideos
    | Keyboard String
    -- === Misc ===
    | RecvCollabState CollabState
    | CollaboratorDisconnected String


type alias OutsideData =
  { tag : String, data: Json.Value }


type alias ExportSettings =
  { format : ExportFormat
  , selection : ExportSelection
  }

type ExportFormat = JSON | TXT
type ExportSelection = All | CurrentSubtree | ColumnNumber Int


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
  , parent : String
  , editing : Maybe String
  , dragModel : DragDrop.Model String DropId
  , draggedTree : Maybe (Tree, String, Int)
  , collaborators : List CollabState
  }


type alias VisibleViewState =
  { active : String
  , editing : Maybe String
  , descendants : List String
  , parent : String
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
