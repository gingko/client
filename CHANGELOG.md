# Change Log
All notable changes to this project will be documented in this file.


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
