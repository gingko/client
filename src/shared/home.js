const {ipcRenderer} = require('electron')
window.Elm= require('../elm/Home')

self.home = Elm.Home.fullscreen()

home.ports.forJS.subscribe(function(elmdata) {
  switch(elmdata){
    case "New":
      // tell electron-start to:
      //   1. call newDb()
      //   2. start an app window with dbname
      ipcRenderer.send('home:new')
      break;
  }
})
