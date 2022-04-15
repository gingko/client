const { test, expect, request } = require('@playwright/test')
const config = require('../../config.js')

const testEmail = 'cypress@testing.com'

test.beforeAll(async () => {
  const auth = Buffer.from(`${config.COUCHDB_ADMIN_USERNAME}:${config.COUCHDB_ADMIN_PASSWORD}`).toString('base64')
  const contextOptions = { extraHTTPHeaders: { authorization: 'Basic ' + auth } }
  const dbAdminRequest = await request.newContext(contextOptions)
  try {
    const userGet = await dbAdminRequest.get(config.TEST_SERVER + '/db/_users/org.couchdb.user:' + testEmail)
    const userResJson = await userGet.json()
    await dbAdminRequest.delete(`${config.TEST_SERVER}/db/_users/org.couchdb.user:${testEmail}?rev=${userResJson._rev}`)
  } catch (e) {
    console.error(e)
  }
})

test.describe('User Signup Flow', async () => {
  test('Can signup using form', async ({ page }) => {
    // Redirects to /signup
    await page.goto(config.TEST_SERVER)
    expect(await page.evaluate('window.location.pathname')).toEqual('/signup')

    // Is focused on the first field
    const signupEmail = page.locator('#signup-email')
    await expect(signupEmail).toBeFocused()

    // Displays errors on submitting empty form
    await page.locator('button.cta').click()

    await expect(page.locator('body'))
      .toContainText(['Please enter an email address', 'Please enter a password'])

    // Creates a new account
    await signupEmail.focus()
    await page.keyboard.type(testEmail.toUpperCase())
    await page.locator('#signup-password').focus()
    await page.keyboard.type('testing')
    await page.locator('#email-optin').click()
    await page.locator('button.cta').click()

    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/)
    expect(await page.locator('#document').count()).toEqual(1)
    await expect(page.locator('#document div.view h1')).toContainText('Welcome to Gingko Writer')

    // Has email verification banner
    await expect(page.locator('#email-confirm-banner')).toContainText('Please confirm your email')

    page.goto(config.TEST_SERVER + '/confirm')

    // Redirected to welcome, Confirmation banner gone
    await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/)
    await expect(page.locator('#document div.view h1')).toContainText('Welcome to Gingko Writer')
    expect(await page.locator('#email-confirm-banner').count()).toEqual(0)

    // Send email confirmation webhook before logging out
    await page.request.post(config.TEST_SERVER + '/mlhooks',
      { data: { events: [{ data: { subscriber: { email: testEmail, confirmation_timestamp: (new Date()).toISOString() } } }] } })
    await page.locator('#account-icon').click()
    await page.locator('#logout-button').click()

    await expect(page).toHaveURL(/\/login/)
    await expect(page.locator('button.cta')).toContainText('Login')
  })
})
