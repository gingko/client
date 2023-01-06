module Types exposing (CardTreeOp(..), Children(..), CollabState, Column, CursorPosition(..), DragExternalModel, DropId(..), Group, HeaderMenuState(..), Mode(..), OutsideData, SidebarMenuState(..), SidebarState(..), SortBy(..), TextCursorInfo, TooltipPosition(..), Tree, ViewMode(..), ViewState, VisibleViewState, dropIdToValue)

import Browser.Dom exposing (Element)
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



-- Tree Ops for Card Based


type CardTreeOp
    = CTIns String String (Maybe String) Int
    | CTUpd String String
    | CTRmv String
    | CTMov String (Maybe String) Int
    | CTMrg String String Bool
    | CTBlk Tree String Int



-- JS Interop


type alias OutsideData =
    { tag : String, data : Enc.Value }



-- Drag and Drop


type DropId
    = Above String
    | Below String
    | Into String


type alias DragExternalModel =
    { dropId : Maybe DropId, isDragging : Bool }


dropIdToValue : DropId -> Enc.Value
dropIdToValue dropId =
    case dropId of
        Above str ->
            Enc.object [ ( "dropPosition", Enc.string "above" ), ( "dropId", Enc.string str ) ]

        Below str ->
            Enc.object [ ( "dropPosition", Enc.string "below" ), ( "dropId", Enc.string str ) ]

        Into str ->
            Enc.object [ ( "dropPosition", Enc.string "into" ), ( "dropId", Enc.string str ) ]



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
    | Account (Maybe Element)


type SortBy
    = Alphabetical
    | ModifiedAt
    | CreatedAt


type HeaderMenuState
    = NoHeaderMenu
    | ExportPreview
    | HistoryView { start : String, currentView : String }
    | Settings


type TooltipPosition
    = RightTooltip
    | LeftTooltip
    | AboveTooltip
    | BelowTooltip
    | BelowLeftTooltip


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
    , dragModel : ( DragDrop.Model String DropId, DragExternalModel )
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
    , dragModel : ( DragDrop.Model String DropId, DragExternalModel )
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
