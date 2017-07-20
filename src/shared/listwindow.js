const jQuery = require('jquery')
window.Elm = require('../elm/ListWindow')


jQuery(document).ready(function() {
  var appDiv = document.getElementById('app')
  self.listWindow = Elm.ListWindow.embed(appDiv, [])

  listWindow.ports.openTree.subscribe( (data) => console.log(data))
})
