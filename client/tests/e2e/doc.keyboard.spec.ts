import { test, expect, setupLifecycleHooks, card } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

test.describe('Keyboard Shortcuts', () => {
  test('Fire in document mode, but not from the editor or the Quick Switcher', async ({ page, login }) => {
    await login();

    // The Cypress version visited `/` first and waited a second before opening
    // the document, to dodge a cold-profile race. `LoadDocument` now waits for
    // the first `trees` sync (see doc.loading.spec.ts), so a direct visit is
    // enough.
    await page.goto(`/${treeIds[0]}`);
    await expect(page.locator('#app-root')).toBeVisible();
    await expect(page.locator('.spinner')).toHaveCount(0);
    await expect(page).toHaveURL(`/${treeIds[0]}`);

    const helpModal = page.locator('.modal.help-modal');
    const modalHeading = page.locator('.modal-header h2');

    await test.step('Document mode: ? toggles help, w toggles word count', async () => {
      await page.keyboard.press('?');
      await expect(helpModal).toBeVisible();
      await page.keyboard.press('?');
      await expect(helpModal).toHaveCount(0);

      await page.keyboard.press('w');
      await expect(modalHeading).toContainText('Word & Character Counts');
      await page.keyboard.press('w');
      await expect(modalHeading).toHaveCount(0);
    });

    await test.step('The editor swallows the shortcut keys', async () => {
      await page.keyboard.press('Enter');
      const editor = page.locator('textarea');
      await expect(editor).toBeFocused();

      await editor.pressSequentially('?', { delay: 30 });
      await expect(helpModal).toHaveCount(0);

      await editor.pressSequentially('w', { delay: 30 });
      await expect(modalHeading).toHaveCount(0);
    });

    await test.step('The Quick Switcher swallows the navigation keys', async () => {
      await page.keyboard.press('Control+Enter');
      await expect(page.locator('textarea')).toHaveCount(0);

      await page.keyboard.press('ArrowRight');
      const activeFirstChild = page.locator(`${card(2, 1, 1)}.active`);
      await expect(activeFirstChild).toBeVisible();

      await page.keyboard.press('Control+o');
      await expect(page.locator('#switcher-modal')).toBeVisible();

      // j / k / h navigate cards in document mode; while the switcher is open
      // they must do nothing, so the same child stays active throughout.
      for (const key of ['j', 'k', 'h']) {
        await page.keyboard.press(key);
        await expect(activeFirstChild).toBeVisible();
      }

      await page.keyboard.press('Escape');
      await expect(page.locator('#switcher-modal')).toHaveCount(0);
    });
  });
});
