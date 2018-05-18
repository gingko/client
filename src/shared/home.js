const {ipcRenderer, dialog, remote} = require('electron')
window.Elm= require('../elm/Home')
const dbMapping = require('./db-mapping')


let docList = dbMapping.getDocList()
var homeWindow = remote.getCurrentWindow()
var home = Elm.Home.fullscreen(docList)

home.ports.forJS.subscribe(function(elmdata) {
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
    case "SetState":
      dbMapping.setState(elmdata.data[0], elmdata.data[1])
      break;
    case "Delete":
      if(confirm("Are you sure you want to delete this?\nThere is no UNDO.")) {
        ipcRenderer.send('home:delete', elmdata.data)
      }
      break;
    default:
      console.log('elmdata', elmdata)
      throw new Error('unexpected input from Elm')
      break;
  }
})
