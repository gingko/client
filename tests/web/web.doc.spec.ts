import sharedDocTest from '../shared/doc.shared.tests'
const { test, expect, request } = require('@playwright/test')
const config = require('../../config.js')

const testEmail = 'cypress@testing.com'
let page

test.beforeAll(async ({ browser }) => {
  const deleteTestUserReq = await request.newContext()
  try {
    await deleteTestUserReq.delete(`${config.TEST_SERVER}/test/user`)
  } catch (e) {
    console.error(e)
  }
  page = await browser.newPage()

  await page.goto(config.TEST_SERVER)
  expect(await page.evaluate('window.location.pathname')).toEqual('/signup')

  // Is focused on the first field
  const signupEmail = page.locator('#signup-email')
  await expect(signupEmail).toBeFocused()
  await page.keyboard.type(testEmail.toUpperCase())
  await page.locator('#signup-password').focus()
  await page.keyboard.type('testing')
  await page.locator('#email-optin').click()
  await page.locator('button.cta').click()

  await expect(page).toHaveURL(/\/[a-zA-Z0-9]{5}$/)
  expect(await page.locator('#document').count()).toEqual(1)
})

test.describe('main file describe block', () => {
  sharedDocTest(page)
})
