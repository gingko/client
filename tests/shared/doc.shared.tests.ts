import { test, expect } from '@playwright/test'

export default function () {
  test('dfasf', ({ page }) => {
    expect(page.locator('textarea')).toBeFocused()
  })
}
