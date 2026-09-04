import { test, expect } from '@playwright/test';
import config from '../../config.js';
import { setupLifecycleHooks, card, group, signupWith } from './shared';
import treeIds from '../../cypress/fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.describe('Document Settings', () => {
  const testEmail = 'cypress@testing.com';

  test.beforeAll(async () => {
    // Signup and seed database with twoTrees data
    await signupWith(testEmail, 'twoTrees');
  });

  test('should load and save settings', async ({ page, context }) => {
    // Login by setting up authentication
    const response = await page.request.post(`${config.TEST_SERVER}/login`, {
      data: { email: testEmail, password: 'testing' }
    });

    expect(response.status()).toBe(200);

    // Set localStorage
    await page.goto(config.TEST_SERVER);
    await page.evaluate(() => {
      localStorage.setItem('gingko-session-storage', JSON.stringify({ email: 'cypress@testing.com', language: 'en' }));
    });

    // Visit the test server again to load with auth
    await page.goto(config.TEST_SERVER);
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
    await page.goto(config.TEST_SERVER);
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

    await page.waitForTimeout(400);

    // Reload
    await page.reload();

    await page.waitForTimeout(400);

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
    await page.waitForTimeout(250);
    await page.keyboard.press('ArrowDown');

    await expect(page.locator(card(2, 1, 2))).toHaveClass(/active/);

    await page.waitForTimeout(4000);

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
