import { test, expect, card, group, expectLastActive, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Document Settings', () => {
  test('should load and save settings', async ({ page, login }) => {
    await login();

    // Visit the test server again to load with auth
    await page.goto('/');
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/);

    // Wait for loading to complete
    await expect(page.locator('.spinner')).not.toBeVisible({ timeout: 10000 });
    await expect(page.locator('text=Synced')).toBeVisible();

    // Can change the document language
    await page.locator('#account-icon').click();
    await page.locator('#language-option').click();
    await page.locator('#lang-es').click();

    // Check for Spanish sync indicator
    await expect(page.locator('#app-root')).toContainText(/Sincronizado|%es:ChangesSynced%/i);

    // Persists language on reload
    await page.goto('/');
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/);
    await expect(page.locator('#app-root')).toContainText(/Sincronizado|%es:ChangesSynced%/i);

    // Saves last active position
    await expect(page.locator('#app-root')).toContainText('Another Test doc');

    // Check initial active card
    await expect(page.locator(card(1, 1, 1))).toHaveClass(/active/);
    await expect(page.locator(group(2, 1))).toHaveClass(/active-descendant/);

    // Select first child
    await page.keyboard.press('ArrowRight');

    await expect(page.locator(card(2, 1, 1))).toHaveClass(/active/);

    await expectLastActive(page, card(2, 1, 1));

    // Reload
    await page.reload();

    // First child should still be selected
    await expect(page.locator(card(2, 1, 1))).toHaveClass(/active/);

    // Open documents sidebar
    await page.locator('#documents-icon').click();

    // Click on 'Untitled' document
    await page.locator('#sidebar-document-list-wrap').getByText('Untitled').click();

    // Verify URL contains the first tree ID
    await expect(page).toHaveURL(new RegExp(treeIds[0]));

    // Check active card state
    await expect(page.locator(card(1, 1, 1))).toHaveClass(/active/);
    await expect(page.locator(group(2, 1))).toHaveClass(/active-descendant/);

    // Select second child
    await page.keyboard.press('ArrowRight');
    await expect(page.locator(card(2, 1, 1))).toHaveClass(/active/);
    await page.keyboard.press('ArrowDown');

    await expect(page.locator(card(2, 1, 2))).toHaveClass(/active/);

    await expectLastActive(page, card(2, 1, 2));

    // Go back to first document
    await page.locator('#sidebar-document-list-wrap').getByText('Another doc, with title').click();

    // First child should still be selected
    await expect(page.locator(card(2, 1, 1))).toHaveClass(/active/);

    // Go back to second document
    await page.locator('#sidebar-document-list-wrap').getByText('Untitled').click();

    // Second child should still be selected
    await expect(page.locator(card(2, 1, 2))).toHaveClass(/active/);
  });
});
