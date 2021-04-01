module Types exposing (Children(..), CollabState, Column, CursorPosition(..), DropId(..), Group, Mode(..), OutsideData, SidebarMenuState(..), SidebarState(..), TextCursorInfo, TooltipPosition(..), Tree, ViewMode(..), ViewState, VisibleViewState)

import Html5.DragDrop as DragDrop
import Json.Encode as Enc
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



-- JS Interop


type alias OutsideData =
    { tag : String, data : Enc.Value }



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


type SidebarState
    = SidebarClosed
    | File


type SidebarMenuState
    = NoSidebarMenu
    | Help
    | Account


type TooltipPosition
    = RightTooltip
    | BelowTooltip


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
    , clipboardTree : Maybe Tree
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
    , isMac : Bool
    }


type alias TextCursorInfo =
    { selected : Bool, position : CursorPosition, text : ( String, String ) }


type CursorPosition
    = Start
    | End
    | Empty
    | Other
