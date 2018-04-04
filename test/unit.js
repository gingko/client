const fs = require('fs')
const {promisify} = require('util')
const path = require('path')
const _ = require('lodash')
const {expect} = require('chai')
const mainjs = require('../app/main.js')


const readdir = promisify(fs.readdir)


let myPause = function(ms) {
  let pausePromise = new Promise((res, rej) => {
    let wait = setTimeout(() => {
      clearTimeout(wait)
      res()
    }, ms)
  })
  return pausePromise
}


describe('File Saving', function() {
  afterEach(async function () {
    await myPause(400)
    let files = await readdir(__dirname)
    files
      .filter(f => f.match(/^testfile-/))
      .map(f => fs.unlink(path.join(__dirname, f)))
  })

  it('should save db to file', async function() {
    let filename = 'testfile-empty-db.txt'
    let filepath = path.join(__dirname, filename)
    let checkfile = function() {
      fs.accessSync(path.join(__dirname, filename))
    }
    await save(filepath)
    expect(checkfile).to.not.throw()
  })

  it('should not have a swap file', async function() {
    let filename = 'testfile-swap.txt'
    let filepath = path.join(__dirname, filename)
    await save(filepath)
    await myPause(400)
    let checkswapfile = function() {
      fs.accessSync(path.join(__dirname, filename + '.swp'))
    }
    expect(checkswapfile, 'swap file still exists').to.throw(/no such file or directory/)
  })
})
