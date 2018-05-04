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
  var db;

  beforeEach(function() {
    db = new PouchDB(new Date().toJSON())
  })

  afterEach(async function () {
    db.destroy()
    await myPause(200)
    let files = await readdir(__dirname)
    let filesToDelete = files.filter(f => f.match(/^testfile-/))
    await Promise.all(filesToDelete.map( f => unlink(path.join(__dirname, f)) ) )
  })

  describe('dbToFile', function() {
    it('should throw an error for invalid filepath', async function () {
      try {
        await fio.dbToFile(db, '!#/not-a-real-filepath')
      } catch (err) {
        expect(err.message).to.include('no such file or directory')
      } finally {
        // cleanup
        db.destroy()
      }
    })

    it('should dump contents to file', async function () {
      let dbname = (await db.info()).db_name
      let filepath = path.join(__dirname, 'testfile-empty-db.txt')
      let result = await fio.dbToFile(db, filepath)

      let filecontents = fs.readFileSync(filepath, 'utf8')
      expect(filecontents).to.include(`"db_name":"${dbname}"`)
    })

    it('should resolve to the saved filepath', async function () {
      let filepath = path.join(__dirname, 'testfile-empty-db.txt')
      let result = await fio.dbToFile(db, filepath)

      expect(result).to.equal(filepath)
    })
  })

  describe('dbFromFile', function() {
    it("should load the docs from the file", async function() {
      let testDocs =
        [ {title: 'A Test Document', _id: 'doc1'}
        , {title: 'A Second Test', _id: 'doc2'}
        ]
      var result = await db.bulkDocs(testDocs)

      let filepath = path.join(__dirname, 'testfile-some-db.txt')
      let dbTwo = new PouchDB(new Date().toJSON())
      await fio.dbToFile(db, filepath)

      dbTwo = await fio.dbFromFile(dbTwo, filepath)
      var resultTwo = await dbTwo.allDocs({ include_docs: true })
      var loadedDocs = resultTwo.rows.map(r => _.omit(r.doc, '_rev'))
      expect(loadedDocs).to.eql(testDocs)

      // cleanup
      dbTwo.destroy()
    })
  })

  describe('getHash', function() {
    it("should get the same hash from two separate dumps", async function() {
      let filepath1 = path.join(__dirname, 'testfile-empty-db1.txt')
      let filepath2 = path.join(__dirname, 'testfile-empty-db2.txt')

      await fio.dbToFile(db, filepath1)
      await fio.dbToFile(db, filepath2)

      var hashResult1 = await fio.getHash(filepath1)
      var hashResult2 = await fio.getHash(filepath2)

      expect(hashResult1).to.equal(hashResult2)
    })
  })

  describe('save', function() {
    it("should throw an error for invalid filepath", async function() {
      try {
        let db = new PouchDB('save-test')
        let filepath = path.join(__dirname, '!#/not-a-real-filepath')
        let hashedResult = await fio.save(db, filepath)
      } catch (err) {
        expect(err.message).to.include('no such file or directory')
      } finally {
        // cleanup
        db.destroy()
      }
    })

    it("should save the file to given filepath", async function() {
      let db = new PouchDB('save-test')
      let filepath = path.join(__dirname, 'testfile-empty-db.txt')
      let saveResult = await fio.save(db, filepath)

      expect(saveResult.path).to.equal(filepath)
      expect(saveResult.hash).to.equal("8zVm2u5FEMnw2sCACFD/Hc0roH0=")

      // cleanup
      db.destroy()
    })

  })
})
