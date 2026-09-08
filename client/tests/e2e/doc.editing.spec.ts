import { test, expect, setupLifecycleHooks, card, group, waitForStableFocus } from './base';

setupLifecycleHooks(test);

// The Cypress version signed a brand-new user up and made a blank tree. The
// closest Playwright equivalent is the empty-document fixture: a clean list, so
// the search assertions below can't see another document's cards.
test.use({ seed: 'empty' });

test.describe('Document Editing', () => {
  test('Can edit a card-based document', async ({ page, login }) => {
    // Lots of typing at delay: 30, plus a dozen-ish sync waits and a full
    // reload. (The pending steps below would push this much higher.)
    test.slow();

    // Escaping out of an edited card pops a confirm(). Playwright dismisses
    // dialogs by default, which would silently *cancel the cancel*, so accept
    // them and keep the messages for assertions.
    const dialogs: string[] = [];
    page.on('dialog', async dialog => {
      dialogs.push(dialog.message());
      await dialog.accept();
    });

    const textarea = page.locator('textarea');
    const saveIndicator = page.locator('#save-indicator');
    const synced = () => expect(saveIndicator).toContainText('Synced');

    /**
     * Creating a card renders its editor twice: once while the card is still
     * unsaved, then again -- as a brand new DOM node -- once it syncs. Anything
     * typed into the first node goes with it when it's swapped out, showing up
     * as a dropped leading character. Wait for focus to land *and* for the
     * focused node to stop being replaced before typing into a new card.
     * See CYPRESS_TO_PLAYWRIGHT_MIGRATION.md, "Creating a Card Renders the
     * Editor Twice".
     */
    async function typeInNewCard(txt: string) {
      await expect(textarea).toBeFocused();
      await waitForStableFocus(page);
      await textarea.pressSequentially(txt, { delay: 30 });
    }

    await login();

    // `#template-new` is the "Blank Tree" template (src/elm/Doc/UI.elm).
    await page.goto('/');
    await page.locator('#new-button').click();
    await page.locator('#templates-block #template-new').click();
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{7}$/);

    // Stands in for cy.url().as('testTreeUrl'); revisited after the rename.
    const treeUrl = page.url();
    const treeId = new URL(treeUrl).pathname.replace(/^\//, '');

    await expect(page.locator('#title-rename')).toHaveValue('Untitled');
    await synced();

    await test.step('Can edit and save a card', async () => {
      await expect(textarea).toBeFocused();
      await textarea.pressSequentially('Hello World :)', { delay: 30 });

      await page.keyboard.press('Control+Enter');

      await expect(page.locator(`${card(1, 1, 1)} .view`)).toContainText('Hello World :)');
      await synced();
    });

    await test.step('Creates a new child on clicking the right + button', async () => {
      const firstCard = page.locator(card(1, 1, 1));
      await expect(firstCard).toHaveClass(/active/);
      await firstCard.hover();

      // `.ins-right` only renders on the active card (viewCardActive in
      // src/elm/Page/Doc.elm), so this stays unique.
      await page.locator('.ins-right').click();
      await typeInNewCard('A child');

      // Saves the card by clicking the checkmark. `.card-btn` alone is
      // ambiguous -- it's also ins-above/right/below, delete and edit.
      await page.locator('.card-btn.save').click();

      await synced();
      await expect(page.locator('div.card.active')).toContainText('A child');
    });

    await test.step("Clicking a different card while editing doesn't duplicate content", async () => {
      await page.locator(card(1, 1, 1)).click();
      await page.keyboard.press('Enter');
      await expect(textarea).toBeFocused();
      await textarea.pressSequentially('XYZ', { delay: 30 });

      await page.locator(card(2, 1, 1)).click();

      await expect(page.locator(card(2, 1, 1))).not.toContainText('XYZ');
      await expect(page.locator(card(1, 1, 1))).toContainText('XYZ');
    });

    await test.step('Clicking outside a card while editing saves it', async () => {
      await page.locator(card(1, 1, 1)).click();
      await page.keyboard.press('Enter');
      await expect(textarea).toBeFocused();
      await textarea.pressSequentially('UVW', { delay: 30 });

      await page.locator('.left-padding-column').click();

      await expect(saveIndicator).not.toContainText('Unsaved Changes...');
      await expect(page.locator(card(1, 1, 1))).not.toHaveClass(/editing/);
      await expect(page.locator(card(1, 1, 1))).toContainText('UVW');
    });

    await test.step('Force a history snapshot of this state', async () => {
      // Setup for the (currently pending) undo/restore steps further down.
      //
      // The server only auto-snapshots a tree on its first push, then not again
      // for 6 hours (takeSnapshotDebounced, leading-edge), so a tree born inside
      // one test never gets a second snapshot on its own. POST /test/snapshot
      // (e2e-only, guarded by TEST_DB_PATH) forces one synchronously -- capturing
      // the two cards that exist right now, before "Another one below" is added.
      await synced();
      const res = await page.request.post('/test/snapshot', { data: { treeId } });
      expect(res.ok()).toBe(true);
    });

    await test.step('Creates and saves a card below using shortcuts', async () => {
      await page.keyboard.press('l');
      await page.keyboard.press('Control+j');

      await typeInNewCard('Another one below');

      await page.keyboard.press('Control+Enter');

      await expect(page.locator('div.card.active')).toContainText('Another one below');
      await synced();
    });

    await test.step('Cancels changes correctly after confirmation', async () => {
      await expect(page.locator('div.card.active')).toBeVisible();
      await page.keyboard.press('Enter');
      await expect(textarea).toBeFocused();
      await textarea.pressSequentially(' changes to cancel xxx', { delay: 30 });

      await page.keyboard.press('Escape');

      // `areYouSureCancel` in src/shared/translation.js.
      await expect
        .poll(() => dialogs)
        .toContain('Are you sure you want to undo your changes?');
      await expect(page.locator('#app-root')).not.toContainText('to cancel xxx');
    });

    await test.step('Can cancel renaming the document', async () => {
      const titleInput = page.locator('#title-rename');
      await expect(titleInput).toBeVisible();
      await titleInput.click();
      await page.keyboard.press('Escape');
      await expect(titleInput).not.toBeFocused();
    });

    await test.step('Can rename the document', async () => {
      const titleInput = page.locator('#title-rename');
      await expect(titleInput).toBeVisible();
      await titleInput.click();
      await expect(titleInput).toBeFocused();

      // Focusing the title selects all of it (SelectAll in src/elm/Page/App.elm),
      // but Playwright's click then collapses that selection to the caret, so an
      // ordinary type would land mid-word. fill() replaces the whole value.
      await titleInput.fill('A new doc title here');
      await titleInput.press('Enter');

      await expect(page).toHaveTitle('A new doc title here - Gingko Writer');
      await expect(titleInput).toHaveValue('A new doc title here');
      await synced();
    });

    await test.step('Has saved the content and activation state across a reload', async () => {
      await page.goto(treeUrl);

      await expect(page.locator(card(2, 1, 2))).toContainText('Another one below');
      await expect(page.locator(card(2, 1, 2))).toBeVisible();

      await expect(page.locator(card(1, 1, 1))).toHaveClass(/ancestor/);

      await expect(page.locator(group(2, 1))).toHaveClass(/has-active/);
      await expect(page.locator(group(2, 1))).not.toHaveClass(/active-descendant/);

      await expect(page.locator(card(2, 1, 2))).toHaveClass(/active/);
    });

    await test.step('Filters cards on search, and unfilters on clearing', async () => {
      // #search-field is a wrapper div; the real input is #search-input.
      const searchInput = page.locator('#search-input');
      await searchInput.click();
      await searchInput.pressSequentially('another', { delay: 30 });

      await expect(page.locator('#document')).not.toContainText('Hello World :)');

      await searchInput.fill('');
      await expect(page.locator('#document')).toContainText('Hello World :)');

      // cy.shortcut typed into `body`; page.keyboard.press types into whatever
      // is focused. Without this blur the Ctrl+Z below would undo the search
      // input's own text instead of opening the history view.
      await searchInput.blur();
    });

    // ────────────────────────────────────────────────────────────────────────
    // PENDING — undo / restore, and every step that depends on the restored
    // state: split down, split up, copy/paste, title shortcuts, formatting
    // shortcuts, card move, and card merge.
    //
    // Driving `#history-slider` from Playwright checks out card-based history
    // versions that render with their cards missing (the column structure is
    // right, the card content is not), so a restore cannot be verified here,
    // and every step below assumes the restored two-card state. The Cypress
    // spec `client/cypress/e2e/doc.editing-cardbased.cy.js` is deliberately
    // kept in place so its (green) run can be compared against this one while
    // the checkout-render behaviour is investigated. `POST /test/snapshot`
    // above is the intended starting point once checkout renders correctly.
    //
    // --- Faithful port of the rest of doc.editing-cardbased.cy.js (pending) ---
    /*
    await test.step('Can move back to a previous version and restore it', async () => {
      await page.keyboard.press('Control+z');
      await expect(page.locator('#history-menu')).toContainText('Restore this Version');

      // TODO: this checks out the forced pre-"Another one below" snapshot. The
      // slider currently renders the checked-out version with its cards missing;
      // resolve that before enabling.
      const slider = page.locator('#history-slider');
      await slider.evaluate((el: HTMLInputElement) => {
        el.value = String(Number(el.max) - 1);
        el.dispatchEvent(new Event('input', { bubbles: true }));
      });

      const rootView = page.locator(`${card(1, 1, 1)} .view`);
      await expect(rootView).toContainText('Hello World :)XYZUVW');
      await expect(page.locator('#document')).toContainText('A child');
      await expect(page.locator('#document')).not.toContainText('Another one below');

      await page.locator('#history-restore').click();
      await expect(page.locator('#history-menu')).toHaveCount(0);
      await synced();

      // Restore brings the two-card version back and drops the third card --
      // verified against real card content, not breadcrumb text.
      await expect(rootView).toContainText('Hello World :)XYZUVW');
      await expect(page.locator(`${card(2, 1, 1)} .view`)).toContainText('A child');
      await expect(page.locator('#document')).not.toContainText('Another one below');
    });

    await test.step('Can split a card down', async () => {
      // A fresh restore leaves Elm's active id pointing at the now-deleted card,
      // so click the restored root to put it in a known state.
      await page.locator(`${card(1, 1, 1)} .view`).click();
      await expect(page.locator(card(1, 1, 1))).toHaveClass(/active/);
      await page.keyboard.press('Enter');
      await expect(textarea).toBeFocused();

      for (let i = 0; i < 7; i++) await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('Control+j');

      await expect(textarea).toHaveValue(')XYZUVW');
      await synced();
      await expect(page.locator(card(1, 1, 1))).not.toContainText(')XYZUVW');

      await page.keyboard.press('Control+Enter');
    });

    await test.step('Can split a card up', async () => {
      await expect(page.locator('div.card.active')).toBeVisible();
      await page.keyboard.press('Enter');
      await expect(textarea).toBeFocused();

      for (let i = 0; i < 3; i++) await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('Control+k');

      await expect(textarea).toHaveValue(')XYZ');
      await page.keyboard.press('Control+Enter');

      await expect(page.locator(card(1, 1, 2))).toContainText(')XYZ');
      await synced();
      await expect(page.locator(card(1, 1, 3))).toContainText('UVW');
    });

    await test.step('Can copy and paste a card', async () => {
      await expect(page.locator(card(2, 1, 1))).toBeVisible();
      await page.locator(card(2, 1, 1)).click();

      await page.keyboard.press('Control+c');
      await page.keyboard.press('ArrowLeft');
      await page.keyboard.press('Control+v');

      await expect(page.locator(card(2, 1, 1))).not.toHaveClass(/active/);
      await expect(page.locator(card(2, 1, 1))).toContainText('A child');
      await expect(page.locator(card(1, 1, 2))).toHaveClass(/active/);
      await expect(page.locator(card(1, 1, 2))).toContainText('A child');
    });

    await test.step('Has working title shortcuts', async () => {
      await page.locator(card(1, 1, 1)).click();
      await page.keyboard.press('Control+ArrowRight');

      await expect(textarea).toBeFocused();
      await waitForStableFocus(page);
      await textarea.pressSequentially('A test title', { delay: 30 });
      await textarea.press('Enter');
      await textarea.press('Enter');
      await textarea.pressSequentially('body', { delay: 30 });

      await textarea.press('Alt+1');
      await expect(textarea).toHaveValue('# A test title\n\nbody');
      await page.keyboard.press('Control+Enter');
      await expect(page.locator('div.view h1')).toContainText('A test title');

      await page.keyboard.press('Enter');
      await textarea.press('Alt+3');
      await expect(textarea).toHaveValue('### A test title\n\nbody');
      await page.keyboard.press('Control+Enter');
      await expect(page.locator('div.view h3')).toContainText('A test title');

      await page.keyboard.press('Enter');
      await textarea.press('Alt+0');
      await expect(textarea).toHaveValue('A test title\n\nbody');
      await page.keyboard.press('Control+Enter');
      await expect(page.locator('div.active p').first()).toContainText('A test title');
    });

    await test.step('Has working formatting shortcuts', async () => {
      await expect(textarea).toHaveCount(0);

      await page.keyboard.press('Control+ArrowDown');
      await typeInNewCard('bold');
      await textarea.press('Control+a');
      await textarea.press('Control+b');
      await expect(textarea).toHaveValue('**bold**');
      await page.keyboard.press('Control+Enter');
      expect(await page.locator(card(2, 1, 3)).innerHTML()).toContain('<strong>bold</strong>');

      await page.keyboard.press('Control+ArrowDown');
      await typeInNewCard('italic');
      await textarea.press('Control+a');
      await textarea.press('Control+i');
      await expect(textarea).toHaveValue('*italic*');
      await page.keyboard.press('Control+Enter');
      expect(await page.locator(card(2, 1, 4)).innerHTML()).toContain('<em>italic</em>');

      await synced();
    });

    await test.step('Can move a card', async () => {
      await expect(page.locator('div.card.active')).toBeVisible();
      await page.keyboard.press('Alt+ArrowUp');
      await synced();

      await expect(page.locator(card(2, 1, 3))).toContainText('italic');
      await expect(page.locator(card(2, 1, 4))).toContainText('bold');
    });

    await test.step('Can merge cards', async () => {
      // First give 1,1,2 / 1,1,3 / 1,1,4 two children each: 1..6 in order.
      const childPairs: [number, string, string][] = [
        [2, '1', '2'],
        [3, '3', '4'],
        [4, '5', '6'],
      ];

      for (const [cardNum, first, second] of childPairs) {
        await expect(page.locator(card(1, 1, cardNum))).toBeVisible();
        await page.locator(card(1, 1, cardNum)).click();

        await page.keyboard.press('Control+ArrowRight');
        await typeInNewCard(first);

        await page.keyboard.press('Control+j');
        await typeInNewCard(second);

        await page.keyboard.press('Control+Enter');
      }

      await synced();

      // Merge 1,1,4 up into 1,1,3. Both are identified by their DOM ids, since
      // the nth-child positions shift as soon as the merge happens.
      const cardId_114 = await page.locator(card(1, 1, 4)).getAttribute('id');
      const cardId_113 = await page.locator(card(1, 1, 3)).getAttribute('id');

      await page.locator(`#${cardId_114}`).click();
      await page.keyboard.press('Control+Shift+ArrowUp');

      await expect(page.locator(`#${cardId_113}`)).toHaveCount(0);
      await expect(page.locator(`#${cardId_114}`)).toHaveClass(/active/);

      await expect(page.locator(group(2, 3))).toHaveClass(/active-descendant/);
      await expect(page.locator(card(2, 3, 1))).toContainText('3');
      await expect(page.locator(card(2, 3, 2))).toContainText('4');
      await expect(page.locator(card(2, 3, 3))).toContainText('5');
      await expect(page.locator(card(2, 3, 4))).toContainText('6');

      // Next merge 1,1,2 down into the new 1,1,3.
      const cardId_112 = await page.locator(card(1, 1, 2)).getAttribute('id');
      const cardId_new_113 = await page.locator(card(1, 1, 3)).getAttribute('id');

      await page.locator(`#${cardId_112}`).click();
      await page.keyboard.press('Control+Shift+ArrowDown');

      await expect(page.locator(`#${cardId_new_113}`)).toHaveCount(0);
      await expect(page.locator(`#${cardId_112}`)).toHaveClass(/active/);

      await expect(page.locator(group(2, 2))).toHaveClass(/active-descendant/);
      for (let i = 1; i <= 6; i++) {
        await expect(page.locator(card(2, 2, i))).toContainText(String(i));
      }
    });
    */
  });
});
