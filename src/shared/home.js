const {ipcRenderer} = require('electron')
window.Elm= require('../elm/Home')
const dbMapping = require('./db-mapping')


let docList = dbMapping.getDocList()
console.log(docList)
self.home = Elm.Home.fullscreen(docList)

home.ports.forJS.subscribe(function(elmdata) {
  console.log('elmdata', elmdata)
  switch(elmdata.tag){
    case "New":
      ipcRenderer.send('home:new')
      break;
    case "Load":
      ipcRenderer.send('home:load', elmdata.data[0], elmdata.data[1])
      break;
    default:
      throw new Error('unexpected input from Elm')
      break;
  }
})
