const { devices } = require('@playwright/test')

const config = {
  timeout: 30 * 1000,
  expect: {
    timeout: 5000
  },
  forbidOnly: !!process.env.CI,
  retries: process.env.CI ? 2 : 0,
  workers: process.env.CI ? 1 : undefined,
  reporter: 'list',
  use: {
    /* Maximum time each action such as `click()` can take. Defaults to 0 (no limit). */
    actionTimeout: 0,
    trace: 'on-first-retry'
  },
  projects:
    [
      {
        name: 'web/chromium',
        use: { ...devices['Desktop Chrome'] },
        testDir: './tests/web'
      }
    ]
}

module.exports = config
