// ***********************************************************
// This example support/index.js is processed and
// loaded automatically before your test files.
//
// This is a great place to put global configuration and
// behavior that modifies Cypress.
//
// You can change the location of this file or turn off
// automatically serving support files with the
// 'supportFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/configuration
// ***********************************************************

// Import commands.js using ES2015 syntax:
import './commands'
import 'cypress-file-upload';

beforeEach(() => {
  const throttle = Cypress.env('throttle')
  if (throttle) {
    cy.intercept('*', (req) => {
        req.continue((res) => {
          // apply a delay of 1 second and a throttle of N kbps
          res.setDelay(1000).setThrottle(parseInt(throttle, 10));
        })
      }
    )
  }
})