import { test as base, expect, type Page } from '@playwright/test';
import { spawn, type ChildProcess } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';

const cwd = process.cwd();
const portBase = Number.parseInt(process.env.PLAYWRIGHT_PORT_BASE ?? '3100', 10);

// The server is native-module heavy (better-sqlite3), so it must run under the
// same Node version its node_modules were built for. Override if that isn't the
// `node` on PATH, e.g. E2E_NODE=~/.nvm/versions/node/v18.12.0/bin/node.
const nodeBin = process.env.E2E_NODE ?? 'node';

export const TEST_EMAIL = 'cypress@testing.com';
export const TEST_PASSWORD = 'testing';

export type SeedName = 'twoTrees' | 'oneTree' | 'oneEmptyTree' | 'fourSmallTrees' | 'empty';

type WorkerOptions = {
  /**
   * Which prebuilt SQLite fixture this worker's server boots against. Set it
   * per spec file with `test.use({ seed: 'oneTree' })`. It is a worker option,
   * so Playwright starts a fresh worker (and a fresh database) whenever the
   * value changes between files.
   */
  seed: SeedName;
  workerSetup: { port: number; dbPath: string };
};

type TestFixtures = {
  /** Logs the test user in against this worker's server. */
  login: () => Promise<void>;
  /**
   * Auto fixture. Keeps the suite off the public internet: the app pulls in
   * Stripe, LogRocket, Google Fonts, gravatar and gingkowriter.com images,
   * which together cost more page-load time than the app itself.
   */
  blockThirdParty: void;
};

function waitForExit(child: ChildProcess): Promise<void> {
  return new Promise(resolve => {
    if (child.exitCode !== null || child.signalCode !== null) {
      resolve();
      return;
    }
    child.once('exit', () => resolve());
  });
}

async function waitForServerReady(
  port: number,
  server: ChildProcess,
  logs: () => string,
) {
  const deadline = Date.now() + 30_000;

  while (Date.now() < deadline) {
    if (server.exitCode !== null || server.signalCode !== null) {
      throw new Error(
        `Test server on port ${port} exited before becoming ready ` +
        `(code ${server.exitCode}, signal ${server.signalCode}).\n${logs()}`
      );
    }

    try {
      const res = await fetch(`http://localhost:${port}/uptime`);
      if (res.ok) return;
    } catch {
      // Not listening yet.
    }

    await new Promise(r => setTimeout(r, 25));
  }

  throw new Error(`Test server on port ${port} was not ready within 30s.\n${logs()}`);
}

export const test = base.extend<TestFixtures, WorkerOptions>({
  seed: ['twoTrees', { option: true, scope: 'worker' }],

  workerSetup: [async ({ seed }, use, workerInfo) => {
    const port = portBase + workerInfo.workerIndex;
    const dbDir = path.join(cwd, 'test-results', 'dbs');
    const dbPath = path.join(dbDir, `db-worker-${workerInfo.workerIndex}.sqlite`);

    // Start from a pristine copy of the fixture. Nothing here ever touches the
    // dev database at data/data.sqlite.
    fs.mkdirSync(dbDir, { recursive: true });
    for (const suffix of ['', '-wal', '-shm']) {
      fs.rmSync(dbPath + suffix, { force: true });
    }
    fs.copyFileSync(path.join(cwd, 'tests', 'e2e', 'fixtures', 'db', `${seed}.sqlite`), dbPath);

    const server = spawn(nodeBin, ['dist/index.js'], {
      cwd: path.join(cwd, '..', 'server'),
      env: {
        ...process.env,
        TEST_DB_PATH: dbPath,
        PORT: String(port),
      },
    });

    let output = '';
    server.stdout?.on('data', d => { output += d; });
    server.stderr?.on('data', d => { output += d; });
    const logs = () => `--- server output (port ${port}) ---\n${output}`;

    try {
      await waitForServerReady(port, server, logs);
      await use({ port, dbPath });
    } finally {
      if (server.exitCode === null && server.signalCode === null) {
        server.kill();
        await waitForExit(server);
      }
    }
  }, { scope: 'worker', auto: true }],

  baseURL: async ({ workerSetup }, use) => {
    await use(`http://localhost:${workerSetup.port}`);
  },

  blockThirdParty: [async ({ context }, use) => {
    await context.route('**/*', async route => {
      const url = new URL(route.request().url());

      // Let the app's own traffic through, including any page-level route a
      // spec registered (those are consulted before this one).
      if (url.hostname === 'localhost' || url.hostname === '127.0.0.1') {
        return route.fallback();
      }

      // doc.js calls Stripe() at module scope, so this one can't just be
      // aborted -- the bundle would throw before the app boots.
      if (url.href.startsWith('https://js.stripe.com/v3/')) {
        return route.fulfill({
          contentType: 'application/javascript',
          body: 'window.Stripe = () => ({ redirectToCheckout: () => {} });',
        });
      }

      return route.abort();
    });

    await use();
  }, { auto: true }],

  login: async ({ page }, use) => {
    await use(async () => {
      const response = await page.request.post('/login', {
        data: { email: TEST_EMAIL, password: TEST_PASSWORD },
      });
      expect(response.status()).toBe(200);

      // localStorage is per-origin, so it has to be seeded against this
      // worker's port rather than restored from a shared storageState file.
      // An init script does that on the test's own first navigation, instead
      // of spending a whole extra page load here just to reach the origin.
      // Only seed when absent: the app writes its own language preference
      // into this key, and re-running on every navigation would clobber it.
      await page.addInitScript(email => {
        if (!localStorage.getItem('gingko-session-storage')) {
          localStorage.setItem(
            'gingko-session-storage',
            JSON.stringify({ email, language: 'en' })
          );
        }
      }, TEST_EMAIL);
    });
  },
});

export { expect };

export function setupLifecycleHooks(t: typeof test) {
  t.afterEach(({}, testInfo) => {
    if (testInfo.status === 'failed' && process.env.CI !== 'true') {
      notify(testInfo.title);
    }
  });
}

function notify(title: string) {
  try {
    spawn('notify-send', ['-u', 'critical', 'Test Failed', title], { stdio: 'ignore' }).unref();
  } catch {
    // Desktop notifications are a convenience; never fail a run over them.
  }
}

export function card(colNum: number, groupNum: number, cardNum: number) {
  return `#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1}) > .card:nth-child(${cardNum})`;
}

export function group(colNum: number, groupNum: number) {
  return `#column-container > .column:nth-child(${colNum}) > .group:nth-child(${groupNum + 1})`;
}

/**
 * Waits until the active-card position for the current document has been
 * persisted.
 *
 * The app writes this synchronously from its ScrollCards handler (see
 * `localStore.set('last-actives', ...)` in src/shared/doc-helpers.js), so
 * polling for the value is the real signal -- no fixed sleep needed.
 *
 * `cardSelector` should point at the card expected to be active; its DOM id is
 * `card-<cardId>` and `last-actives` stores the bare id.
 */
export async function expectLastActive(page: Page, cardSelector: string) {
  const domId = await page.locator(cardSelector).getAttribute('id');
  expect(domId).toMatch(/^card-/);
  const cardId = domId!.replace(/^card-/, '');
  const treeId = new URL(page.url()).pathname.replace(/^\//, '');

  await expect
    .poll(() => page.evaluate(
      id => JSON.parse(localStorage.getItem(`gingko-local-store/${id}/settings`) || '{}')['last-actives'],
      treeId
    ))
    .toContain(cardId);
}
