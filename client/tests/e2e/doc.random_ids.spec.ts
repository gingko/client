import { test, expect, setupLifecycleHooks, waitForStableFocus } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Random seed initialization', () => {
  test('Should not duplicate ids', async ({ page, login }) => {
    await login();

    await page.goto(`/${treeIds[0]}`);

    await expect(page.locator('#app-root')).toBeVisible();
    await expect(page.locator('.spinner')).not.toBeVisible();
    await expect(page).toHaveURL(new RegExp(treeIds[0]));

    // Select "Another Child card" and add a new card below it.
    await page.locator('#column-container .card .view', { hasText: 'Another Child card' }).click();
    await page.keyboard.press('Control+ArrowDown');

    // The new card's editor renders once while unsaved and then again as a
    // brand new DOM node once it syncs; wait for the focused node to settle
    // before typing (see CYPRESS_TO_PLAYWRIGHT_MIGRATION.md).
    const textarea = page.locator('textarea');
    await expect(page.locator('#save-indicator')).toContainText('Synced');
    await expect(textarea).toBeFocused();
    await waitForStableFocus(page);
    await textarea.pressSequentially('newmod', { delay: 30 });
    await page.keyboard.press('Control+Enter');

    await expect(page.locator('#save-indicator')).toContainText('Synced');

    // Switch to the other tree and back.
    await page.locator('#documents-icon').click();
    await page.locator('#sidebar-document-list-wrap').getByText('Another doc, with title').click();
    await expect(page).toHaveURL(new RegExp(treeIds[1]));
    await expect(page.locator('#title')).toContainText('Another doc, with title');

    await page.locator('#sidebar-document-list-wrap').getByText('Untitled').click();
    await expect(page).toHaveURL(new RegExp(treeIds[0]));

    // Re-select the card we added; a duplicated id would render its editor
    // twice, so assert there is exactly one textarea.
    await page.locator('#column-container .card .view', { hasText: 'newmod' }).click();
    await page.keyboard.press('Control+ArrowDown');
    await expect(page.locator('textarea')).toHaveCount(1);
  });
});
