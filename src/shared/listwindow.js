const {ipcRenderer, remote} = require('electron')
const app = remote.app
const jQuery = require('jquery')
const path = require('path')
const mkdirp = require('mkdirp')
const PouchDB = require('pouchdb')

window.Elm = require('../elm/ListWindow')

const dbpath = path.join(app.getPath('appData'), 'data', 'treelist')
mkdirp.sync(dbpath)
self.db = new PouchDB(dbpath)

jQuery(document).ready(function() {
  var appDiv = document.getElementById('app')

  db.allDocs(
    { include_docs: true
    }).then(function (result) {
      data = result.rows.map(r => r.doc)

      dataTuples = data.map(d => [d._id, d.name])
      self.listWindow = Elm.ListWindow.embed(appDiv, dataTuples)

      listWindow.ports.openTree.subscribe( (data) => {
        db.put({_id: data[0], name: data[1]})
        ipcRenderer.send('openTree', data)
      })
    })

})
