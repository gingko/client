const jQuery = require('jquery')
window.Elm = require('../elm/ListWindow')


jQuery(document).ready(function() {
  var appDiv = document.getElementById('app')
  console.log(appDiv)
  self.listWindow = Elm.ListWindow.embed(appDiv, [])
})
