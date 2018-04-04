const electron = require('electron')
const {expect} = require('chai')


describe('Clipboard', function() {
  it('should write and read from clipboard', function() {
    electron.clipboard.writeText('Hello World!')
    expect(electron.clipboard.readText()).to.be.equal('Hello World!')
  })
})
