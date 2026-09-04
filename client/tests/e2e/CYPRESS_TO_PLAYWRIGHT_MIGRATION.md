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

### 1. Custom Element Timing (`gw-textarea`)

**Critical:** The `gw-textarea` custom element requires special handling for focus and input readiness.

```typescript
// ❌ FLAKY - Focus state doesn't guarantee input readiness
await page.locator('#mbtn-add-right').click();
await expect(textarea).toBeFocused();
await textarea.pressSequentially('text', { delay: 30 });

// ✅ RELIABLE - Wait for value to be set (ensures connectedCallback complete)
await page.locator('#mbtn-add-right').click();
await expect(textarea).toHaveValue(/.*/);  // Wait for any value
await textarea.pressSequentially('text', { delay: 30 });
```

**Why:** The custom element's `connectedCallback()` sets the initial value at line 80 of `src/shared/doc-helpers.js`. Waiting for this ensures event listeners are attached and the element is ready for input.

### 2. Card Button Overlays

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

### 3. Save Indicator Pattern

Always wait for the save indicator after typing to ensure data is synced:

```typescript
await page.locator('textarea').pressSequentially('text', { delay: 30 });
await expect(page.locator('#save-indicator')).toContainText('Synced');
```

### 4. Mobile Button Workflows

When testing mobile buttons that create new cards:

```typescript
// Pattern for mobile add buttons
await page.locator('#mbtn-add-right').click();
await expect(textarea).toHaveValue(/.*/);  // Wait for custom element initialization
await textarea.pressSequentially('text', { delay: 30 });
await page.locator('#mbtn-save').click();

// Assert on the .view element, not the card
const cardView = page.locator('#column-container > .column:nth-child(2) .view');
await expect(cardView).toContainText('text');
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
- [ ] **Use `.toHaveValue(/.*/)` instead of `.toBeFocused()` for textarea waits**
- [ ] **Query `.view` div for card content assertions, not the card element**
- [ ] Add `setupLifecycleHooks(test)` if needed (from `shared.ts`)
- [ ] Use `storageState` for authenticated tests
- [ ] Test the migration by running multiple times
- [ ] Delete the old Cypress test file

## Example Migration

See `tests/e2e/doc.ui.spec.ts` for a complete example of a migrated test.

**Before (Cypress):** `cypress/e2e/doc.ui.cy.js`
**After (Playwright):** `tests/e2e/doc.ui.spec.ts`

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
