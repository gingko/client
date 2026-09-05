import { test, expect, setupLifecycleHooks } from './base';
import treeIds from './fixtures/fourSmallTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'fourSmallTrees' });

test.describe('Managing Documents', () => {
  test('Navigates correctly', async ({ page, login }) => {
    // Playwright dismisses dialogs by default, which would silently cancel the
    // delete confirm below. Accept them, and keep the messages for assertions.
    const dialogs: string[] = [];
    page.on('dialog', async dialog => {
      dialogs.push(dialog.message());
      await dialog.accept();
    });

    const docItems = page.locator('#sidebar-document-list-wrap .sidebar-document-item');
    const docNames = page.locator('#sidebar-document-list .sidebar-document-item a');

    await login();

    await test.step('Should navigate to last edited tree', async () => {
      await page.goto('/');
      await expect(page).toHaveURL(`/${treeIds[1]}`);

      await expect(page.locator('.spinner')).not.toBeVisible({ timeout: 20000 });
      await expect(page.locator('#app-root')).toContainText('123');

      await page.locator('#documents-icon').click();

      await expect(docItems).toHaveCount(4);
      await expect(page.locator('#sidebar-document-list-wrap .sidebar-document-item.active'))
        .toContainText('tree-1');
      await expect(page.locator('#title')).toContainText('tree-1');

      // Prevent navigating away if editing
      await page.keyboard.press('Enter');
      await page.locator('textarea').pressSequentially('chhh', { delay: 30 });

      await docItems.last().click();

      await expect
        .poll(() => dialogs)
        .toContain('You have unsaved changes!\nCtrl+Enter to save.');
      await expect(page).toHaveURL(`/${treeIds[1]}`);
    });

    await test.step('Should have working sidebar and Quick Switcher', async () => {
      await page.goto(`/${treeIds[1]}`);
      await expect(page).toHaveURL(`/${treeIds[1]}`);

      await expect(page.locator('.spinner')).not.toBeVisible({ timeout: 20000 });

      const listWrap = page.locator('#sidebar-document-list-wrap');
      await expect(listWrap).toContainText('tree-u');
      await expect(listWrap).toContainText('tree-a');

      // Go to uvw doc
      await listWrap.getByText('tree-u').click();
      await expect(page.locator('#document')).toContainText('uvw');

      // Go to another doc
      await listWrap.getByText('tree-a').click();
      await expect(page).toHaveURL(`/${treeIds[0]}`);
      await expect(page.locator('#app-root')).toContainText('abc');

      await test.step('Sorts documents correctly', async () => {
        await expect(docNames).toHaveText(['tree-1', 'tree-u', 'tree-a', 'tree-x']);

        await page.locator('#sort-alphabetical').click();
        await expect(docNames).toHaveText(['tree-1', 'tree-a', 'tree-u', 'tree-x']);

        await page.locator('#sort-created').click();
        await expect(docNames).toHaveText(['tree-x', 'tree-a', 'tree-u', 'tree-1']);

        await page.locator('#sort-modified').click();
        await expect(docNames).toHaveText(['tree-1', 'tree-u', 'tree-a', 'tree-x']);
      });

      await test.step('Filters by name correctly in sidebar', async () => {
        const filter = page.locator('#document-list-filter');

        await filter.pressSequentially('-u', { delay: 30 });

        await expect(docItems).toHaveCount(1);
        await expect(docItems).toContainText('tree-u');
        await expect(listWrap).not.toContainText('tree-a');
        await expect(listWrap).not.toContainText('tree-1');

        await filter.pressSequentially('x', { delay: 30 });

        await expect(page.locator('#no-documents')).toBeVisible();
        await expect(docItems).toHaveCount(0);

        await filter.fill('');

        await expect(docItems).toHaveCount(4);
      });

      await test.step('Has a working context menu', async () => {
        const secondDoc = page.locator('#sidebar-document-list-wrap .sidebar-document-item:nth-child(2)');
        const contextMenu = page.locator('#sidebar-context-menu');

        // Menu opens on right click
        await secondDoc.click({ button: 'right' });
        await expect(contextMenu).toContainText('Delete Tree');

        // Should close context menu on clicking elsewhere
        await page.locator('#sidebar-context-overlay').click();
        await expect(contextMenu).not.toBeVisible();

        // Open menu again, and delete the tree (the confirm() is auto-accepted)
        await secondDoc.click({ button: 'right' });
        await contextMenu.getByText('Delete Tree').click();

        await expect(docItems).toHaveCount(3);
        await expect(listWrap).not.toContainText('tree-u');
        await expect(contextMenu).not.toBeVisible();
      });

      await test.step('Quick Switcher', async () => {
        const modal = page.locator('#switcher-modal');
        const switcherItems = modal.locator('.switcher-document-list .switcher-document-item');

        // Toggles switcher modal on Ctrl+O, and autofocuses its input
        await expect(modal).not.toBeVisible();

        await page.keyboard.press('Control+o');
        await expect(modal).toBeVisible();
        await expect(modal.locator('input')).toBeFocused();

        await page.keyboard.press('Control+o');
        await expect(modal).not.toBeVisible();

        // Check contents
        await page.keyboard.press('Control+o');
        await expect(switcherItems).toHaveText(['tree-a', 'tree-1', 'tree-x']);
        await expect(switcherItems.first()).toHaveClass(/current/);
        await expect(switcherItems.first()).toHaveClass(/selected/);

        // Check list navigation
        await page.keyboard.press('ArrowDown');
        await page.keyboard.press('ArrowDown');
        await page.keyboard.press('ArrowUp');

        await expect(switcherItems.nth(0)).toHaveClass(/current/);
        await expect(switcherItems.nth(0)).not.toHaveClass(/selected/);
        await expect(switcherItems.nth(1)).toHaveClass(/selected/);
        await expect(switcherItems.nth(2)).not.toHaveClass(/selected/);

        // Test filtering
        await modal.locator('input').pressSequentially('x', { delay: 30 });

        const switcherList = modal.locator('.switcher-document-list');
        await expect(switcherList).toContainText('tree-x');
        await expect(switcherList).not.toContainText('tree-a');
        await expect(switcherList).not.toContainText('tree-1');

        // Should close on Esc
        await page.keyboard.press('Escape');
        await expect(modal).not.toBeVisible();
        await expect(page.locator('#app-root')).toContainText('abc');

        // Should go to selected tree on Enter
        await page.keyboard.press('Control+o');
        await page.keyboard.press('ArrowDown');
        await page.keyboard.press('Enter');
        await expect(page).toHaveURL(`/${treeIds[1]}`);

        // Should select first tree when filtering
        await page.keyboard.press('Control+o');
        await expect(modal).toBeVisible();
        await modal.locator('input').pressSequentially('x', { delay: 30 });

        await expect(switcherItems).toHaveCount(1);
        await expect(switcherItems.first()).toHaveClass(/selected/);
        await expect(switcherItems.first()).not.toHaveClass(/current/);

        await page.keyboard.press('Enter');
        await expect(page).toHaveURL(`/${treeIds[2]}`);
        await expect(page.locator('#app-root')).toContainText('xyz');
      });
    });
  });
});
