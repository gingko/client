const { defineConfig } = require('cypress')

module.exports = defineConfig({
  'cypress-watch-and-reload': {
    watch: 'web/*.js',
  },
  defaultCommandTimeout: 12000,
  video: false,
  e2e: {
    // We've imported your old cypress plugins here.
    // You may want to clean this up later by importing these.
    setupNodeEvents(on, config) {
      return require('./cypress/plugins/index.js')(on, config)
    },
  },
})
