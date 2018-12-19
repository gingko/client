# Change Log
All notable changes to this project will be documented in this file.
"Unreleased" changes are implemented but haven't been released yet.

## Unreleased
  - Fix: Couldn't click on search area since 2.2.6
  - Fix: Delete & Move shortcuts not always shown in shortcut tray

## [2.3.0] - 2018-12-11
  - Fix "Error: Cannot find module '../screen'" on clicking New Blank
  - Fix Unhandled error on updater when offline
  - (Internal) Revert to electron v2

## [2.2.9] - 2018-12-06
  - Fix "Unsaved" confirmation not bringing document to focus
  - Fix Undo/Redo not working from menu
  - Add Alt-shortcuts to menu

## [2.2.8] - 2018-12-05
  - (Internal) Upgrade dependencies, including electron to 3.0
  - Fix "Error: Cannot find module '../screen'"

## [2.2.7] - 2018-12-04
  - Fix error introduced in last version on opening New documents
  - Enable Undo/Redo options in Edit menu

## [2.2.6] - 2018-12-03
  - Undo/Redo and basic version history navigation
  - Fix non-persistent "last active card" on opening a document

## [2.2.5] - 2018-11-01
  - Fix error when "Save As" to same location
  - Fix error when cancelling "Save As"

## [2.2.4] - 2018-09-18
  - Fix loading error "processNextTick is not a function" for legacy .gko

## [2.2.3] - 2018-09-06
  - Fix loading error "processNextTick is not a function"

## [2.2.2] - 2018-09-04
  - Add RTL language support
  - Add `w` to toggle wordcount tray
  - Fix mistaken "Unsaved" if changes are undone before saving card
  - Fix "Unhandled error" when opening a non-existent file
  - Fix long URLs overflowing the card

## [2.2.1] - 2018-08-28
  - Fixed Zoom In/Zoom Out "Unhandled error"s
  - Fixed "Unhandled error" when taking actions on already-closed window
  - Fixed multiple periods in filename causing Home screen crash
  - Fixed missing blank line after lists

## [2.2.0] - 2018-08-26
  - (Major) Autosave to files, instead of internal folder

## [2.1.5] - 2018-07-19
  - Added Search & Filtering

## [2.1.4] - 2018-07-12
  - Fixed Save errors on rapid moves/changes
  - Fixed title reverts to Untitled" on rename
  - Fixed file extension filtering issues on macOS
  - Fixed JSON import not saving unless modified
  - Fixed single linebreaks ignored in Word exports
  - Fixed cancelling card didn't reset save indicator
  - Added link to github issues to "Help" menu
  - Moved 'Saved' indicator to top left

## [2.1.3] - 2018-07-10
  - Saving "Reference Error": Increased save-delay from 1s to 3s
  - "File > Repeat Last Export" for quick backups/updates
  - "File > Save" option added, with info error

## [2.1.2] - 2018-06-27
  - (Critical) Fixed error on saving rapid changes
  - Added Window menu to macOS

## [2.1.1] - 2018-06-26
  - Added Shortcut hints for formatting
  - Added CtrlOrCmd + Enter to start editing
  - Fixed code blocks from overflowing
  - Fixed Home/End key bug introduced in 2.1.0
  - Fixed Select All not working on macOS

## [2.1.0] - 2018-06-19
  - (Major) Autosave to disk on every change

## [2.0.11] - 2018-04-30
  - (Internal) Improved error-handling

## [2.0.10] - 2018-04-28
  - Added Export to MS Word
  - Added cut/copy/paste of cards and subtrees.
  - Fixed: Alt+arrows now move by word on macOS

## [2.0.9] - 2018-04-26
  - Fixed auto-update errors on Linux.
  - Added highlighting of all ancestors, not just parent.
  - Scroll to active card on window resizing.
  - Prevented dragging of last card in tree.
  - Fixed Undo/Redo/Cut/Copy/Paste in Edit menu on Mac.

## [2.0.8] - 2018-04-26
  - Icons working on all three platforms.
  - Correct certificate used to sign Windows builds.

## [2.0.7] - 2018-04-24
  - Added highlighting of parent card.
  - Fixed bug with "Export Current and Children" (was exporting "Whole Tree")
  - Fixed missing Linux icon in menus.

## [2.0.6] - 2018-04-20
  - (Internal) Rewrote Saving and "Save Confirmation" logic, for more relability.

## [2.0.5] - 2018-04-04
  - (Critical) Fixed bug that led to overwrite when doing: Open, New, File > Save.

## [2.0.4] - 2018-04-02
  - (Critical) Fixed bug that didn't ask to save changes on New/Load/Import/Exit.
  - Fixed bug that led to needing to force-quit.

## [2.0.3] - 2018-03-31
  - Added "Export Column" and "Export Current and Children" options.
  - Fixed line endings on Windows (Export as Text was all on one-line).
  - Continue from the last active card when opening a file.

## [2.0.2] - 2018-03-28
  - Fixed Ctrl vs Cmd issue with shortcut tray.
  - Added more guidance to first-run message.

## [2.0.1] - 2018-03-27
  - Launch version bump.
  - Update electron-builder to fix Linux auto-updater.

## [0.8.9] - 2018-03-20
  - Welcome crisp message added, welcome video removed.
  - Window state remembered and restored on reopening app.

## [0.8.8] - 2018-03-19
  - Welcome video added.
  - Crisp.chat added as in-app support solution.

## [0.8.7] - 2018-03-17
  - Files now saved to swap file, and only copied to original if integrity check passes.
  - (Internal) Status and Head recovery attempts.
  - (Internal) Upgrade to electron 2 beta.

## [0.8.6] - 2018-03-14
  - (Critical) Fixed bug with version history data, that caused some data corruption.

## [0.8.5] - 2018-02-27
  - (Internal) Build process reworked, including code signing certificates.

## [0.8.4] - 2018-02-13
  - (Critical) If closed quickly after saving, data could be corrupted. Now Fixed.
  - Added basic word count feature (total and session).
  - Added basic syntax highlighting.
  - Ctrl+arrows (used for text navigation) no longer trigger card insertions.
  - Fixed trial counter (used to max out at "20 days left").
  - (Internal) Refactored Ports

## [0.8.3] - 2018-01-10
  - Added basic drag-and-drop for card moving.
  - Added Alt+hjkl for card moving.
  - Fixed non-wrapping for `backtick` verbatim sections.
  - Fixed issue where only text edits triggered "file changed" state.
  - Fixed bug where clicking other cards while editing resulted in lost changes.

## [0.8.2] - 2017-12-17
  - Re-enabled autoUpdater. Fixed startup errors on macOS.

## [0.8.1] - 2017-12-10
  - Removed autoUpdater temporarily, to fix unhandled exception on startup.

## [0.8.0] - 2017-12-06
  - Added "Export to Text" options.
  - Fixed "Contact Adriano" menu option (was blank screen, now opens mail client)
  - Added Free Trial limits and path to purchase.

## [0.2.1] - 2017-10-24
  - Added confirmation dialog for cancelling edits, preventing accidental loss of words.
  - Fixed: Bug where clicking on links would open within Gingko, with no way to escape.

## [0.2.0] - 2017-10-19
  - Implemented auto-update for Linux and Windows.

## [0.1.7] - 2017-09-19
  - Fixed bug with JSON import (unable to Save As .gko unless changes made first).

## [0.1.6] - 2017-09-12
  - Fixed JSON export. Format was incorrect and didn't import to web version.
  - Save by clicking checkmark now works.
  - Replaced Drift with simpler email + phone support.
  - Columns widths increased to 450px minimum.
  - Fixed horizontal scrolling bug from 0.1.5 (was out of sync with columns).
  - Readded ability to zoom-in/zoom-out.

## [0.1.5] - 2017-08-29
  - Mac: Cut/Copy/Paste added to Menu. Should fix "can't copy/paste" on macOS.
  - Export to JSON feature available again.
  - Fixed "Can't add cards above/below first card".
  - Fixed "Changes lost if I create new card while in edit mode" bug.
  - Removed unused "License/Purchase" options from Help menu.
  - Fixed "Contact Support" not working.


## [0.1.4] - 2017-08-08
- **Completely rewrote** storage to be able to support:
  - Git-like branching version history
  - Real-time collaboration when online
  - Change merging when syncing after offline editing
  - Backup to CouchDB server
- Removed auto-titles.


## [0.1.3] - 2017-01-16
- User info from download added to Intercom (name & contact email).
- Can now save document while editing card.
- Fixed Markdown export when using autoheadings.
- Fixed textareas not growing when text hits bottom, when first created.
- Second attempt to fix crash when opening external links.
- Upgrade Electron framework from 1.4.3 to 1.4.14


## [0.1.2] - 2017-01-10
- Fixed crash when clicking on http links within app.
- Send anonymous user data to Intercom (number of saves, version number, etc).


## [0.1.1] - 2017-01-05
- Installed Intercom instead, for smoother "Contact Support" experience.


## [0.1.0] - 2017-01-05
- Added link for direct support chat with me.


## [0.0.15] - 2017-01-03
- Added occasional request for payment (Pay What You Want).


## [0.0.14] - 2016-12-19
- Readded Undo/Redo functionality.


## [0.0.13] - 2016-12-19
- Core rewrite for performance improvements.
- **Forced to remove Undo/Redo temporarily**.


## [0.0.12] - 2016-12-06
- Greatly improved performance on large trees.
- Fixed bug: As of 0.0.10, wasn't able to "File > New". Now works.
- Temporarily (?) removed "fade background" effect while editing, for performance.


## [0.0.11] - 2016-12-05
- Temporarily removed buggy "save backup while typing".
- Slight performance improvement via lazier rendering. Still laggy on large trees.


## [0.0.10] - 2016-11-30
- Recovery file: on any card insertion/deletion/moves, and every ~10 characters.
- Warn on loading a file if a recovery file is also found.


## [0.0.9] - 2016-11-24
- Menu fixed on macOS (wasn't showing "File|Gingko", "Edit", "View", or "Debug" menus).
- Fixed: Onload "File not found" error for macOS.
- InsertAbove/Below commands when on Root card no longer creates children.
- Cards now have max-height on edit mode, then switch to scroll. 
Prevents scrolling bugs with long cards.
- Ctrl/Cmd arrows in edit mode no longer create cards
(shortcut conflicted with text navigation).
- Pressing Tab in edit mode inserts two spaces (previously: lost focus).
- Code blocks (triple backtick) now preserve whitespace.


## [0.0.8] - 2016-11-18
- Fixed import bug: importing from web app "Copy" operation worked,
but when importing from web app "Export", only first card was imported.
- Added Ctrl+Arrows to create cards, in addition to Ctrl+JKL.


## [0.0.7] - 2016-11-17
- Min-width for columns, horizontal scrolling.


## [0.0.6] - 2016-11-17
- Bug fix: when deep into tree, navigating up or down beyond group boundaries,
didn't work, now does.
- Bug fix: images now scale down to fit in cards.


## [0.0.5] - 2016-11-15
- You can now import trees from GingkoApp.com's JSON format.


## [0.0.4] - 2016-11-15
- To prevent losing unsaved card changes, pressing 'Esc' to cancel changes
now asks for confirmation.


## [0.0.3] - 2016-11-11
- Added automatic titles based on first line of edit area.
First column has h1 headings, second column has h2, etc.
Unless overridden with Markdown "## Headings".
- Fixed bug where clicking "Insert Above|Below|Child" buttons while editing
would blank current card.
- Minor visual changes to card, especially in edit mode.
