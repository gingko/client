const {ipcRenderer, dialog, remote} = require('electron')
window.Elm= require('../elm/Home')
const dbMapping = require('./db-mapping')


let docList = dbMapping.getDocList()
var homeWindow = remote.getCurrentWindow()
var home = Elm.Home.fullscreen(docList)

home.ports.forJS.subscribe(function(elmdata) {
  console.log('elmdata', elmdata)
  switch(elmdata.tag){
    case "New":
      ipcRenderer.send('home:new')
      break;
    case "ImportGko":
      ipcRenderer.send('home:import-gko')
      break;
    case "Load":
      ipcRenderer.send('home:load', elmdata.data[0], elmdata.data[1])
      break;
    default:
      throw new Error('unexpected input from Elm')
      break;
  }
})
