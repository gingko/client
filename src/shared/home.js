window.Elm= require('../elm/Home')

self.home = Elm.Home.fullscreen()

home.ports.forJS.subscribe(function(elmdata) {
  switch(elmdata){
    case "New":
      // call newDb()
      // tell electron-start to start an app window
      // with dbname
      break;
  }
})
