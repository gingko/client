module Types exposing (Children(..), CollabState, Column, Conflict, CursorPosition(..), DropId(..), Group, Mode(..), Op(..), Selection(..), TextCursorInfo, Tree, ViewMode(..), ViewState, VisibleViewState)

import Html5.DragDrop as DragDrop
import Translation


type alias Tree =
    { id : String
    , content : String
    , children : Children
    }


type Children
    = Children (List Tree)


type alias Group =
    List Tree


type alias Column =
    List (List Tree)



-- Drag and Drop


type DropId
    = Above String
    | Below String
    | Into String



-- Transient View States


type ViewMode
    = Normal
    | Editing
    | FullscreenEditing


type alias CollabState =
    { uid : String
    , mode : Mode
    , field : String
    }


type Mode
    = CollabActive String
    | CollabEditing String


type alias ViewState =
    { active : String
    , activePast : List String
    , descendants : List String
    , ancestors : List String
    , viewMode : ViewMode
    , searchField : Maybe String
    , dragModel : DragDrop.Model String DropId
    , draggedTree : Maybe ( Tree, String, Int )
    , copiedTree : Maybe Tree
    , collaborators : List CollabState
    }


type alias VisibleViewState =
    { active : String
    , viewMode : ViewMode
    , descendants : List String
    , ancestors : List String
    , dragModel : DragDrop.Model String DropId
    , collaborators : List CollabState
    , language : Translation.Language
    }


type alias TextCursorInfo =
    { selected : Bool, position : CursorPosition, text : ( String, String ) }


type CursorPosition
    = Start
    | End
    | Empty
    | Other



-- Conflict Resolution


type Selection
    = Original
    | Ours
    | Theirs
    | Manual


type alias Conflict =
    { id : String
    , opA : Op
    , opB : Op
    , selection : Selection
    , resolved : Bool
    }



-- Operations ( ? )


type Op
    = Ins String String (List String) Int
    | Mod String (List String) String String
    | Del String (List String)
    | Mov String (List String) Int (List String) Int
