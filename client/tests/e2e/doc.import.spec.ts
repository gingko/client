import type { Page } from '@playwright/test';
import { test, expect, card, setupLifecycleHooks } from './base';
import treeIds from './fixtures/twoTrees.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'twoTrees' });

/** Card text, minus the '+' insert buttons that overlay every card. */
function cardView(page: Page, colNum: number, groupNum: number, cardNum: number) {
  return page.locator(`${card(colNum, groupNum, cardNum)} .view`);
}

/** Runs one import from the template selector, with the given splitting option. */
async function importFiles(page: Page, splitOption: string) {
  await page.locator('#new-icon').click();
  await page.locator('#template-import-text').click();
  await page.locator('#import-text-file-input').click();
  await page.locator(`#${splitOption}`).click();
  await page.locator('#import-text-perform').click();
}

test.describe('Text Imports from Startup State', () => {
  test('Imports text files', async ({ page, login }) => {
    await login();

    // The app can't open a real file picker under test, so it hands back a
    // canned set of files instead (see IntegrationTestEvent in
    // src/shared/doc.js). The counter that walks through those sets lives on
    // window, so this test must not reload the page partway through.
    await page.addInitScript(() => { (window as any).__E2E__ = true; });

    // Starts at the root, as the original Cypress test did: the import flow
    // begins from whichever document you happen to have open.
    await page.goto('/');
    await expect(page).toHaveURL(`/${treeIds[1]}`);
    await expect(page.locator('.spinner')).not.toBeVisible();
    await expect(page.locator('#app-root')).toContainText('Another Test doc');

    await test.step('Brings up the Import Modal on clicking', async () => {
      await page.locator('#new-icon').click();
      await page.locator('#template-import-text').click();

      const modal = page.locator('#import-text-modal');
      await expect(modal).toBeVisible();
      await expect(page.locator('.modal-header')).toContainText('Import Text Files');

      // Goes back to template selector on close button
      await page.locator('.close-button').click();
      await expect(page.locator('#template-import-text')).toBeVisible();

      await page.locator('#template-import-text').click();
      await expect(modal).toBeVisible();
    });

    await test.step('Toggles the splitting options', async () => {
      const byParagraph = page.locator('#split-by-paragraph');
      const noSplitting = page.locator('#no-splitting');

      await expect(byParagraph).toBeChecked();
      await expect(noSplitting).not.toBeChecked();

      await noSplitting.click();
      await expect(noSplitting).toBeChecked();
      await expect(byParagraph).not.toBeChecked();

      await byParagraph.click();
      await expect(byParagraph).toBeChecked();
    });

    await test.step('Imports a text file with split-by-paragraph', async () => {
      await page.locator('#import-text-file-input').click();
      await expect(page.locator('li.file-item')).toContainText('foo.txt');

      await page.locator('#import-text-perform').click();

      // Importing one file titles the document after the filename
      await expect(page.locator('#title-rename')).toHaveValue('foo');

      await expect(cardView(page, 1, 1, 1)).toContainText('This is a test file.');
      await expect(cardView(page, 1, 1, 1)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 1, 1, 4)).toContainText('And a split break.');
    });

    await test.step('Imports a text file with split-by-separator', async () => {
      await importFiles(page, 'split-by-separator');

      await expect(page.locator('#title-rename')).toHaveValue('foo2');

      await expect(cardView(page, 1, 1, 1)).toContainText('Test file two.');
      await expect(cardView(page, 1, 1, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 1, 1, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 1, 1, 2)).toContainText('And a split break.');
    });

    await test.step('Imports multiple files, one per card', async () => {
      await importFiles(page, 'no-splitting');

      await expect(page.locator('#title-rename')).toHaveValue('Untitled');

      await expect(cardView(page, 1, 1, 1)).toContainText('bar1');
      await expect(cardView(page, 2, 1, 1)).toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 1)).toContainText('And a split break.');

      await expect(cardView(page, 1, 1, 2)).toContainText('bar2');
      await expect(cardView(page, 2, 2, 1)).toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 1)).toContainText('And a split break.');
    });

    await test.step('Imports multiple files, split by paragraph', async () => {
      await importFiles(page, 'split-by-paragraph');

      await expect(page.locator('#title-rename')).toHaveValue('Untitled');

      await expect(cardView(page, 1, 1, 1)).toContainText('bar1');
      await expect(cardView(page, 2, 1, 1)).toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 1)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 2)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 1, 4)).not.toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 4)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 4)).toContainText('And a split break.');

      await expect(cardView(page, 1, 1, 2)).toContainText('bar2');
      await expect(cardView(page, 2, 2, 1)).toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 1)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 2)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 2, 4)).not.toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 4)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 4)).toContainText('And a split break.');
    });

    await test.step('Imports multiple files, split by separator ---', async () => {
      await importFiles(page, 'split-by-separator');

      await expect(page.locator('#title-rename')).toHaveValue('Untitled');

      await expect(cardView(page, 1, 1, 1)).toContainText('bar1');
      await expect(cardView(page, 2, 1, 1)).toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('Test file three.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 2)).toContainText('And a split break.');

      await expect(cardView(page, 1, 1, 2)).toContainText('bar2');
      await expect(cardView(page, 2, 2, 1)).toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('Test file four.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 2)).toContainText('And a split break.');
    });

    await test.step('Imports multiple files, split by separator !g', async () => {
      await page.locator('#new-icon').click();
      await page.locator('#template-import-text').click();
      await page.locator('#import-text-file-input').click();
      await page.locator('#split-by-separator').click();
      await page.locator('#separator-input').fill('!g');
      await page.locator('#import-text-perform').click();

      await expect(page.locator('#title-rename')).toHaveValue('Untitled');

      await expect(cardView(page, 1, 1, 1)).toContainText('baz1');
      await expect(cardView(page, 2, 1, 1)).toContainText('Test file five.');
      await expect(cardView(page, 2, 1, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('Test file five.');
      await expect(cardView(page, 2, 1, 2)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 1, 2)).toContainText('And a split break.');

      await expect(cardView(page, 1, 1, 2)).toContainText('baz2');
      await expect(cardView(page, 2, 2, 1)).toContainText('Test file six.');
      await expect(cardView(page, 2, 2, 1)).toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 1)).not.toContainText('And a split break.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('Test file six.');
      await expect(cardView(page, 2, 2, 2)).not.toContainText('With a paragraph break.');
      await expect(cardView(page, 2, 2, 2)).toContainText('And a split break.');
    });
  });
});
