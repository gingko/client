import type { Page } from '@playwright/test';
import { test, expect, card, setupLifecycleHooks } from './base';
import treeIds from './fixtures/oneTree.ids.json';

setupLifecycleHooks(test);

test.use({ seed: 'oneTree' });

type ExportFormat = 'word' | 'text' | 'json';

type PreviewExpectations = {
  contains: string[];
  excludes?: string[];
};

/**
 * Switches the export preview to `format` and checks its contents.
 *
 * The preview re-renders asynchronously whenever the format or the selection
 * changes, so the positive assertions are polled; they are what tells us the
 * new content has landed. The negative ones are then checked against that same
 * settled content -- on their own they'd pass against a half-rendered preview.
 *
 * The Word preview is real HTML, the others are plain text in a nested div, so
 * they're compared against innerHTML and textContent respectively.
 */
async function checkPreview(page: Page, format: ExportFormat, expected: PreviewExpectations) {
  await page.locator(`#export-format-${format}`).click();

  const preview = page.locator('#export-preview');
  const read = async () =>
    (format === 'word' ? await preview.innerHTML() : await preview.textContent()) ?? '';

  await expect
    .poll(async () => {
      const content = await read();
      return expected.contains.filter(snippet => !content.includes(snippet));
    }, { message: `missing snippets in ${format} export preview` })
    .toEqual([]);

  const content = await read();
  for (const snippet of expected.excludes ?? []) {
    expect(content, `unexpected snippet in ${format} export preview`).not.toContain(snippet);
  }
}

// Snippets from the Romeo & Juliet tree, by where they sit in it.
const actI = { word: '<h1 id="act-i">Act I</h1>', text: '# Act I', json: '"content": "# Act I' };
const prologue = {
  word: '<h2 id="act-1-prologue">Act 1, Prologue</h2>',
  text: '## Act 1, Prologue',
  json: '"content": "## Act 1, Prologue',
};
const sampson = {
  word: '<p><strong>Sampson</strong></p>\n<p>Gregory, o\' my word, we\'ll not carry coals.</p>',
  text: '**Sampson**\n\nGregory, o\' my word, we\'ll not carry coals.',
  json: '"content": "**Sampson**\\n\\nGregory, o\' my word, we\'ll not carry coals.',
};
const scene3 = {
  word: '<h2 id="act-1-scene-3">Act 1, Scene 3</h2>',
  text: '## Act 1, Scene 3',
  json: '"content": "## Act 1, Scene 3',
};
const ladyCapulet = {
  word: '<p><strong>Lady Capulet</strong></p>\n<p>Enough of this; I pray thee, hold thy peace.</p>',
  text: '**Lady Capulet**\n\nEnough of this; I pray thee, hold thy peace.',
  json: '"content": "**Lady Capulet**\\n\\nEnough of this; I pray thee, hold thy peace.',
};

test.describe('Document Exporting', () => {
  // Runs in ~8s locally against the 873-card Romeo & Juliet fixture -- loading
  // that tree plus nine polled preview re-renders. The default 15s budget left
  // too little headroom on the shared CI runner (2 workers, slower CPU), where
  // it timed out mid-test; 30s still catches a gross load- or render-time
  // regression on a large tree without flaking on runner contention.
  test('Exports correctly', async ({ page, login }) => {
    test.setTimeout(30_000);

    await login();

    await page.goto(`/${treeIds[0]}`);
    await expect(page).toHaveURL(`/${treeIds[0]}`);
    await expect(page.locator('div.spinner')).not.toBeVisible({ timeout: 20000 });
    await expect(page.locator('#app-root')).toContainText('Two noble families', { timeout: 20000 });

    // Select a mid-column card
    await expect(page.locator(card(1, 1, 1))).toHaveClass(/active/);

    const scene3Card = page.locator(card(2, 1, 4));
    await expect.poll(() => scene3Card.innerHTML()).toContain(scene3.word);
    await scene3Card.click();
    await expect(scene3Card).toHaveClass(/active/);

    // Try various export options (preview only -- nothing is downloaded)
    await page.locator('#export-icon').click();

    await test.step('Whole Tree', async () => {
      await page.locator('#export-select-all').click();

      await checkPreview(page, 'word', { contains: [actI.word, prologue.word, sampson.word] });
      await checkPreview(page, 'text', { contains: [actI.text, prologue.text, sampson.text] });
      await checkPreview(page, 'json', { contains: [actI.json, prologue.json, sampson.json] });
    });

    await test.step('Current Card & Subtree', async () => {
      await page.locator('#export-select-subtree').click();

      await checkPreview(page, 'word', {
        contains: [scene3.word, ladyCapulet.word],
        excludes: [actI.word, prologue.word, sampson.word],
      });
      await checkPreview(page, 'text', {
        contains: [scene3.text, ladyCapulet.text],
        excludes: [actI.text, prologue.text, sampson.text],
      });
      await checkPreview(page, 'json', {
        contains: [scene3.json, ladyCapulet.json],
        excludes: [actI.json, prologue.json, sampson.json],
      });
    });

    await test.step('Current Column', async () => {
      await page.locator('#export-select-column').click();

      await checkPreview(page, 'word', {
        contains: [prologue.word, scene3.word],
        excludes: [actI.word, sampson.word, ladyCapulet.word],
      });
      await checkPreview(page, 'text', {
        contains: [prologue.text, scene3.text],
        excludes: [actI.text, sampson.text, ladyCapulet.text],
      });
      await checkPreview(page, 'json', {
        contains: [prologue.json, scene3.json],
        excludes: [actI.json, sampson.json, ladyCapulet.json],
      });
    });
  });
});
