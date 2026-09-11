# Cypress to Playwright Migration Guide

This guide documents the process and best practices for migrating Cypress tests to Playwright in the Gingko codebase.

## Table of Contents
- [Overview](#overview)
- [Key Syntax Differences](#key-syntax-differences)
- [Custom Commands Migration](#custom-commands-migration)
- [Common Patterns](#common-patterns)
- [Gingko-Specific Considerations](#gingko-specific-considerations)
- [Testing Tips](#testing-tips)
- [Migration Checklist](#migration-checklist)

## Overview

### Why Playwright?
- Faster execution (~50% faster than Cypress in our tests)
- Better handling of modern web apps
- Built-in TypeScript support
- More reliable auto-waiting mechanisms

### Setup
Playwright is already configured in this codebase. Tests should be placed in `tests/e2e/` with the `.spec.ts` extension.

## Key Syntax Differences

### Test Structure
```typescript
// Cypress
describe('Feature', () => {
  it('does something', () => {
    cy.visit('/path')
  })
})

// Playwright
import { test, expect } from '@playwright/test';

test.describe('Feature', () => {
  test('does something', async ({ page }) => {
    await page.goto('/path')
  })
})
```

### Assertions
```typescript
// Cypress
cy.get('#element').should('be.visible')
cy.get('#element').should('contain', 'text')
cy.url().should('match', /pattern/)

// Playwright
await expect(page.locator('#element')).toBeVisible()
await expect(page.locator('#element')).toContainText('text')
await expect(page).toHaveURL(/pattern/)
```

### Element Interaction
```typescript
// Cypress
cy.get('#button').click()
cy.get('input').type('text')

// Playwright
await page.locator('#button').click()
await page.locator('input').fill('text')
// OR for sequential typing with delays:
await page.locator('input').pressSequentially('text', { delay: 30 })
```

### Waiting
```typescript
// Cypress
cy.wait(1000)
cy.contains('text')  // auto-waits

// Playwright
await page.waitForTimeout(1000)  // Avoid if possible
await expect(page.locator('text=text')).toBeVisible()  // Preferred
```

## Custom Commands Migration

### Cypress Custom Commands
The Cypress tests use several custom commands defined in `cypress/support/commands.js`:

#### `cy.writeInCard(text)`
```typescript
// Cypress
cy.writeInCard('some text')

// Playwright
await page.locator('textarea').pressSequentially('some text', { delay: 30 })
```

#### `cy.shortcut(keys)`
```typescript
// Cypress
cy.shortcut('{ctrl}{enter}')

// Playwright
await page.keyboard.press('Control+Enter')
```

#### `cy.getColumn(colNum)`
```typescript
// Cypress
cy.getColumn(2)

// Playwright
page.locator(`#column-container > .column:nth-child(${colNum})`)
```

#### `cy.getCard(colNum, groupNum, cardNum)`
```typescript
// Cypress
cy.getCard(2, 1, 2)

// Playwright
// Use the helper from shared.ts:
import { card } from './shared';
page.locator(card(2, 1, 2))

// Or inline:
page.locator(`#column-container > .column:nth-child(2) > .group:nth-child(${1 + 1}) > .card:nth-child(2)`)
```
**Note:** Group numbers are 0-indexed in the helper but 1-indexed in CSS nth-child.

#### `cy.deleteUser(email)` and `cy.signup(email)`
```typescript
// Cypress
cy.deleteUser('test@example.com')
cy.signup('test@example.com')

// Playwright
// Use the auth.setup.ts approach with storageState
test.use({ storageState: `${process.cwd()}/tests/e2e/.auth/user.json` });
```

## Common Patterns

### Viewport Changes
```typescript
// Cypress
cy.viewport(360, 640)

// Playwright
await page.setViewportSize({ width: 360, height: 640 })
```

### Network Interception
```typescript
// Cypress
cy.intercept('/api/endpoint', { statusCode: 200, body: {} }).as('apiCall')
cy.wait('@apiCall')

// Playwright
await page.route('/api/endpoint', async route => {
  await route.fulfill({ status: 200, body: JSON.stringify({}) });
});
```

### Keyboard Shortcuts
```typescript
// Cypress
cy.get('body').type('{ctrl}{enter}')
cy.get('body').type('{esc}')

// Playwright
await page.keyboard.press('Control+Enter')
await page.keyboard.press('Escape')
```

### Multiple Locators
```typescript
// Cypress
cy.get('.card').first()
cy.get('.card').eq(2)

// Playwright
page.locator('.card').first()
page.locator('.card').nth(2)
```

## Gingko-Specific Considerations

### 1. Creating a Card Renders the Editor Twice

**Critical:** Creating a card renders its editor once while the card is still
unsaved, and then again -- as a **brand new DOM node** -- once it syncs. Anything
typed into the first node goes with it when it's swapped out, which shows up as
a dropped leading character: `lak` arrives as `ak`.

```typescript
// ❌ FLAKY - focus lands on the first node, which is about to be replaced
await page.locator('#mbtn-add-right').click();
await expect(textarea).toBeFocused();
await textarea.pressSequentially('text', { delay: 30 });

// ⚠️ STILL FLAKY - waiting for "Synced" only helps if the indicator wasn't
// already showing "Synced" from an earlier action. If it was, this resolves
// instantly and waits for nothing -- which is exactly what happens creating
// a second or third card in a row.
await page.locator('#mbtn-add-right').click();
await expect(page.locator('#save-indicator')).toContainText('Synced');
await expect(textarea).toBeFocused();
await textarea.pressSequentially('text', { delay: 30 });

// ✅ RELIABLE - wait for the focused node itself to stop being replaced,
// regardless of what the indicator says.
await page.locator('#mbtn-add-right').click();
await expect(page.locator('#save-indicator')).toContainText('Synced');
await expect(textarea).toBeFocused();
await waitForStableFocus(page); // from ./base
await textarea.pressSequentially('text', { delay: 30 });
```

Verified with a MutationObserver under 20x CPU throttling: the first `<textarea>`
appears while `#save-indicator` reads "Saved Offline", and a second, different
node replaces it when the indicator reaches "Synced". `waitForStableFocus`
(in `base.ts`) marks the live focused element and polls for the marker's
survival, which catches the swap even when the indicator text itself doesn't
change.

**Do not** reach for `toHaveValue(/.*/)` here, which earlier revisions of this
guide recommended. A new card's editor is empty, `/.*/` matches the empty
string, and the assertion passes instantly without waiting for anything. Editing
an *existing* card doesn't create anything, so it needs no such wait.

### 2. Entering Fullscreen Drops the First Keystroke

**Critical:** Elm re-renders on the *next animation frame*, so the click or
`Shift+Enter` that enters fullscreen returns while the card's own `<textarea>` is
still in the DOM and still focused. The fullscreen editor does not self-focus
(the `gw-textarea` custom element focuses only when `!this.isFullscreen`, see
`src/shared/doc-helpers.js`) -- focus arrives a beat later via Elm's
`Browser.Dom.focus` task. A `page.locator(':focus')` resolved in that window
points at the dying node, and the first character typed into it vanishes with
it: `' line'` lands as `line`.

```typescript
// ❌ FLAKY - :focus still resolves to the card's textarea, which is about to
// be swapped out; the leading space is lost.
await page.locator('.fullscreen-card-btn').click();
await focused.pressSequentially(' line', { delay: 30 });

// ⚠️ STILL FLAKY - #fullscreen-main is visible before its editor is focused.
await page.keyboard.press('Shift+Enter');
await expect(page.locator('#fullscreen-main')).toBeVisible();
await focused.pressSequentially('lmn', { delay: 30 });

// ✅ RELIABLE - wait for the fullscreen editor itself to hold focus.
await page.locator('.fullscreen-card-btn').click();
await waitForFullscreenEditor(page); // local helper in doc.fullscreen.spec.ts
await focused.pressSequentially(' line', { delay: 30 });
```

`waitForFullscreenEditor` (a local helper in `doc.fullscreen.spec.ts`, the only
spec that enters fullscreen) asserts on `#fullscreen-main textarea:focus`, which
is only satisfied once fullscreen has rendered *and* focus has landed.
`waitForStableFocus` is the wrong tool here -- it can mark `document.body` during
the focus gap and return with focus still on `body`.

### 3. Card Button Overlays

**Critical:** Card elements contain button overlays that appear in text content assertions.

```typescript
// ❌ INCORRECT - Includes '+' button text from card-btn elements
const card = page.locator('#column-container > .column:nth-child(2) > .card:nth-child(1)');
await expect(card).toContainText('expected text');  // May see "expected text+++"

// ✅ CORRECT - Query the .view div which contains only card content
const card = page.locator('#column-container > .column:nth-child(2) > .card:nth-child(1) .view');
await expect(card).toContainText('expected text');
```

**Card DOM Structure:**
```html
<div class="card">
  <span class="card-btn ins-above">+</span>
  <span class="card-btn ins-right">+</span>
  <span class="card-btn ins-below">+</span>
  <div class="view"><!-- actual card content --></div>
</div>
```

### 4. Save Indicator Pattern

Always wait for the save indicator after typing to ensure data is synced:

```typescript
await page.locator('textarea').pressSequentially('text', { delay: 30 });
await expect(page.locator('#save-indicator')).toContainText('Synced');
```

### 5. Mobile Button Workflows

When testing mobile buttons that create new cards:

```typescript
// Pattern for mobile add buttons
await page.locator('#mbtn-add-right').click();
await expect(page.locator('#save-indicator')).toContainText('Synced');  // See #1
await expect(textarea).toBeFocused();
await textarea.pressSequentially('text', { delay: 30 });
await page.locator('#mbtn-save').click();

// Assert on the .view element, not the card
const cardView = page.locator('#column-container > .column:nth-child(2) .view');
await expect(cardView).toContainText('text');
```

### 6. Cold Browser Profiles

Every Playwright test starts with empty IndexedDB, which Cypress tests never
did -- `cy.signup_with` visited the app during setup, priming the local
document list before any test navigated. Anything that reads from Dexie on
first paint is therefore racing the websocket sync in a way it never used to.

Migrating the export test surfaced exactly this: `LoadDocument` declared a
document missing before the document list had arrived, so the first visit to
`/<treeId>` landed on `/<treeId>/404-not-found`. That's fixed (`LoadDocument`
now waits for the first `trees` sync), and `doc.loading.spec.ts` guards it.

The lesson generalises: when a migrated test fails only on a cold profile,
suspect a real first-run bug before reaching for a workaround.

### 7. The File-Picker Stand-In (`window.__E2E__`)

No test runner can drive the OS file picker, so `IntegrationTestEvent` in
`src/shared/doc.js` hands the app a canned set of files instead. It's gated on
`window.Cypress || window.__E2E__`, so a Playwright test has to opt in:

```typescript
await page.addInitScript(() => { (window as any).__E2E__ = true; });
```

The counter that walks through the successive file sets lives on `window`, so a
test that reloads the page partway through starts over from the first set.

**Remember to rebuild the bundle (`bun esbuild.mjs`) after touching
`src/shared/*.js`** -- the server serves `web/doc.js`, not the source.

### 8. The Signup Flow (`auth.spec.ts`)

Signing up from scratch needs things the other specs get for free:

- **A user-less database.** Every other fixture already contains
  `cypress@testing.com`, so `POST /signup` would 409. `auth.spec.ts` uses
  `test.use({ seed: 'noUser' })` -- `fixtures/db/noUser.sqlite`, made from
  `empty.sqlite` with `DELETE FROM users`.
- **CouchDB cleanup between runs.** `POST /signup` creates a CouchDB database
  `userdb-<hex(email)>` that outlives the per-test SQLite fixture copy. If it's
  still there from a previous run, signup *hangs* rather than erroring. The
  spec's first `test.step` calls `DELETE /test/user` to drop it; it's in the
  test (not a `beforeAll`) so a CI retry re-runs it too. This also means the
  Playwright suite now needs CouchDB on `:5984`.
- **No real email.** `POST /forgot-password` calls Mailgun server-side, which no
  browser route can intercept. `server/src/index.ts` routes every send through a
  `sendEmail` helper that no-ops when `E2E_NO_EMAIL=true`; `base.ts` sets that on
  the test server it spawns. After touching `server/src/*.ts`, rebuild:
  `cd server && npm run build` (Node 18 -- `nvm use 18`).

The old `GET /db/userdb-...` assertions from the Cypress test were dropped: the
app reads its data from SQLite now, so that request no longer proves anything.

### 9. Card-Based History Is Entirely Local -- No Server Snapshot Needed

`doc.editing.spec.ts`'s undo/restore step doesn't force a server-side snapshot.
It doesn't need to: every save that adds or removes a card (`SaveCardBased` in
`src/shared/doc.js`) already writes a full tree snapshot straight into
IndexedDB (`dexie.tree_snapshots`), and the history slider/restore UI is built
from that table alone. The server's own snapshotting (`takeSnapshotDebounced`,
6-hour leading-edge) is a separate, independent mechanism that this UI doesn't
depend on. An earlier revision of this test assumed otherwise and added a
`POST /test/snapshot` e2e-only server hook to force one -- that hook turned out
to be unnecessary and was removed.

While chasing this, a real (if narrow) bug surfaced and was fixed: the
snapshot-capture code queried `dexie.cards` directly without deduping to each
card's latest row. `dexie.cards` is an append-only log (a new row per edit,
keyed by `updatedAt`), so any card edited more than once ended up with
stale/duplicate rows baked into its history snapshot. Fixed in
`src/shared/doc.js` (`SaveCardBased`) by deduping the same way
`saveBackupToImmortalDB`/`getTreeString` already did.

### 10. History Restore Leaves the Column View Stale

After clicking `#history-restore`, IndexedDB is correctly updated (verified
directly against `dexie.cards`) and the breadcrumb trail (built straight from
`workingTree.tree`) reflects the restored content immediately -- but `#document`
itself renders empty. Elm's `workingTree.columns` (what the card/column view
actually reads) doesn't get rebuilt from the post-restore tree until something
else forces it; a page reload does, ordinary DOM events after Restore don't.
This is real app behaviour, not a Playwright timing artifact -- it reproduces
with a plain keyboard `Control+z` → `#history-restore` click, no slider
manipulation involved.

The Cypress original never catches this: it only asserts against `#app-root`
text (which the breadcrumb alone satisfies) and never re-checks real card
selectors until well after a page reload two steps later ("Has saved the
content... across a reload" happens *before* the history section, not after
restore). `doc.editing.spec.ts` works around the same way Cypress accidentally
does -- reload (`page.goto(treeUrl)`) right after restore -- but asserts against
real card selectors (`card(1,1,1)`, `card(2,1,1)`) instead of loose text, which
is a stronger check than the Cypress version ever made.

### 11. Copy/Paste Needs an Explicit Clipboard Permission Grant

`mod+c`/`mod+v` round-trip through the *real* system clipboard
(`navigator.clipboard.writeText`/`readText` in `src/shared/doc.js`), unlike
every other keyboard shortcut in the editor, which is all internal Elm state.
Without granting clipboard permissions, `readText()` rejects (denied) and paste
silently no-ops -- no error, no dialog, just nothing happens. Cypress doesn't
need this because Chrome-under-Cypress grants clipboard access to the test
origin by default; Playwright doesn't. Fix:

```typescript
await page.context().grantPermissions(['clipboard-read', 'clipboard-write']);
```

## Testing Tips

### 1. Use `pressSequentially` for Realistic Typing

```typescript
// Simulates human typing with delays between keystrokes
await page.locator('textarea').pressSequentially('Hello world', { delay: 30 });
```

This is more realistic than `.fill()` and helps catch timing issues.

### 2. Leverage Auto-Waiting

Playwright automatically waits for elements. Don't add explicit waits unless necessary:

```typescript
// ❌ Not needed
await page.waitForSelector('#element');
await page.locator('#element').click();

// ✅ Better - auto-waits
await page.locator('#element').click();
```

### 3. Use Role-Based Selectors When Possible

```typescript
// More resilient to DOM changes
await page.getByRole('button', { name: 'Submit' }).click()
await page.getByRole('heading', { name: 'Keyboard Shortcuts' })
```

### 4. Avoid Strict Mode Violations

If a text selector matches multiple elements, use more specific selectors:

```typescript
// ❌ May match multiple elements
page.locator('text=Keyboard Shortcuts')

// ✅ More specific
page.getByRole('heading', { name: 'Keyboard Shortcuts' })
```

### 5. Test Isolation with Storage State

Use the auth setup pattern for authenticated tests:

```typescript
import { test, expect } from '@playwright/test';

test.use({ storageState: `${process.cwd()}/tests/e2e/.auth/user.json` });

test('authenticated test', async ({ page }) => {
  // User already logged in from auth.setup.ts
});
```

### 6. Verify Non-Flakiness

Run tests multiple times to ensure they're reliable:

```bash
for i in {1..10}; do npx playwright test your-test.spec.ts || break; done
```

## Migration Checklist

When migrating a Cypress test to Playwright:

- [ ] Convert test structure to Playwright syntax (`test.describe`, async/await)
- [ ] Update imports to use `@playwright/test`
- [ ] Replace `cy.get()` with `page.locator()`
- [ ] Replace Cypress assertions with Playwright `expect()`
- [ ] Update custom commands (writeInCard, shortcut, getCard, etc.)
- [ ] Use `pressSequentially` instead of Cypress `.type()`
- [ ] Replace `cy.intercept` with `page.route`
- [ ] Update viewport changes to `setViewportSize`
- [ ] **Wait for `#save-indicator` to read "Synced" before typing into a newly created card**
- [ ] **Query `.view` div for card content assertions, not the card element**
- [ ] Add `setupLifecycleHooks(test)` if needed (from `shared.ts`)
- [ ] Set `test.use({ seed: '...' })` if the `twoTrees` default isn't the right
      starting fixture. Each test gets its own server and database copy keyed on
      the spec file's name -- nothing else to configure (see `fixtures/README.md`)
- [ ] Use `storageState` for authenticated tests
- [ ] Test the migration by running multiple times
- [ ] Delete the old Cypress test file

## Example Migration

See `tests/e2e/doc.ui.spec.ts` for a complete example of a migrated test.

**Before (Cypress):** `cypress/e2e/doc.ui.cy.js`
**After (Playwright):** `tests/e2e/doc.ui.spec.ts`

`tests/e2e/doc.export.spec.ts` is a good example of turning nested Cypress
`describe` blocks into `test.step`s, and of checking content that renders
asynchronously (see its `checkPreview` helper: poll on what *should* appear,
then assert what shouldn't against that settled content -- a bare negative
assertion will happily pass against a preview that hasn't re-rendered yet).

Key improvements:
- ~50% faster execution (7-8s vs 10s)
- More reliable (no flaky failures)
- Better TypeScript support
- Clearer async/await patterns

## Resources

- [Playwright Documentation](https://playwright.dev/)
- [Playwright vs Cypress](https://playwright.dev/docs/why-playwright)
- Local files:
  - `tests/e2e/shared.ts` - Helper functions
  - `tests/e2e/auth.setup.ts` - Authentication setup
  - `playwright.config.ts` - Configuration
