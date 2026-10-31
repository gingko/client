import { DatabaseSync } from 'node:sqlite';
import { test, expect, setupLifecycleHooks } from './base';
import type { Page } from '@playwright/test';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

// Signup hangs if its CouchDB database already exists, and these tests only need
// the row that the tree_collaborators foreign key points at.
function addUser(dbPath: string, email: string) {
  const db = new DatabaseSync(dbPath);
  db.prepare('INSERT INTO users (id) VALUES (?)').run(email);
  db.close();
}

async function openCollabModal(page: Page) {
  await page.goto('/');
  await expect(page.locator('.spinner')).not.toBeVisible({ timeout: 20000 });
  await page.locator('#collab-button').click();
  await expect(page.locator('#add-collab-input')).toBeVisible();
}

async function grantAccess(page: Page, email: string) {
  await page.locator('#add-collab-input').fill(email);
  await page.getByText('Grant access', { exact: true }).click();
}

test.describe('Collaborators modal', () => {
  test('Shows an error for an address with no account', async ({ page, login }) => {
    await login();
    await openCollabModal(page);

    await grantAccess(page, 'nobody@example.com');

    const error = page.getByText('No Gingko account found for that email.');
    await expect(error).toBeVisible();
    // Fails if the toast is stacked behind the modal or its overlay.
    await error.click({ trial: true });
  });

  test('Adds a registered address typed in mixed case', async ({ page, login, testServer }) => {
    addUser(testServer.dbPath, 'friend@example.com');
    await login();
    await openCollabModal(page);

    await grantAccess(page, 'Friend@Example.com');

    await expect(page.locator('#collab-modal span', { hasText: 'friend@example.com' })).toBeVisible();
  });
});
