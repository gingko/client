const fs = require('fs')
const {promisify} = require('util')
const path = require('path')
const _ = require('lodash')
const {expect} = require('chai')
const fio = require("../src/shared/file-io.js")


const PouchDB = require("pouchdb-browser");
const replicationStream = require('pouchdb-replication-stream')
PouchDB.plugin(replicationStream.plugin)
PouchDB.adapter('writableStream', replicationStream.adapters.writableStream)
const memoryAdapter = require("pouchdb-adapter-memory")
PouchDB.plugin(memoryAdapter)


const readdir = promisify(fs.readdir)
const readFile = promisify(fs.readFile)
const unlink = promisify(fs.unlink)


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
    let filesToDelete = files.filter(f => f.match(/^testfile-/))
    await Promise.all(filesToDelete.map( f => unlink(path.join(__dirname, f)) ) )
  })

  it('should throw an error for invalid filepath', async function () {
    let db = new PouchDB('somedbname')

    try {
      await fio.dbToFile(db, '!#/not-a-real-filepath')
    } catch (err) {
      expect(err.message).to.include('no such file or directory')
    }
  })

  it('should dump contents to file', async function () {
    let dbname = `somedb-${Date.now()}`
    let db = new PouchDB(dbname)
    let filepath = path.join(__dirname, 'testfile-empty-db.txt')
    await fio.dbToFile(db, filepath)

    let filecontents = fs.readFileSync(filepath, 'utf8')
    expect(filecontents).to.include(`"db_name":"${dbname}"`)
  })

  xit('should save db to file', async function() {
    let filename = 'testfile-empty-db.txt'
    let filepath = path.join(__dirname, filename)
    let checkfile = function() {
      fs.accessSync(path.join(__dirname, filename))
    }
    await save(filepath)
    expect(checkfile).to.not.throw()
  })

  xit('should not have a swap file', async function() {
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
