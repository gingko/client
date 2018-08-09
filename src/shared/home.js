const {ipcRenderer, dialog, remote} = require('electron')
window.Elm= require('../elm/Home')
const dbMapping = require('./db-mapping')


var crisp_loaded = false;

$crisp.push(['do', 'chat:hide'])
$crisp.push(['on', 'session:loaded', () => { crisp_loaded = true }])
$crisp.push(['on', 'chat:closed', () => { $crisp.push(['do', 'chat:hide']) }])
$crisp.push(['on', 'chat:opened', () => { $crisp.push(['do', 'chat:show']) }])
$crisp.push(['on', 'message:received', () => { $crisp.push(['do', 'chat:show']) }])

ipcRenderer.on('menu-contact-support', () => { if(crisp_loaded) { $crisp.push(['do', 'chat:open']); $crisp.push(['do', 'chat:show']); } else { shell.openExternal('mailto:adriano@gingkoapp.com') } } )

ipcRenderer.on("doc-list-reload", () => {
  let docList = dbMapping.getDocList();
  home.ports.docListReload.send(docList);
});


let docList = dbMapping.getDocList()
var homeWindow = remote.getCurrentWindow()
var home = Elm.Home.fullscreen([Date.now(), docList])

home.ports.forJS.subscribe(function(elmdata) {
  switch(elmdata.tag){
    case "New":
      ipcRenderer.send('home:new')
      break;
    case "ImportGko":
      ipcRenderer.send('home:import-file')
      break;
    case "Load":
      ipcRenderer.send('home:load', elmdata.data[0], elmdata.data[1])
      break;
    case "OpenOther":
      ipcRenderer.send("home:open-other");
      break;
    case "SetState":
      dbMapping.setState(elmdata.data[0], elmdata.data[1])
      break;
    case "Delete":
      if(confirm("Are you sure you want to delete this?\nThere is NO UNDO.")) {
        ipcRenderer.send('home:delete', elmdata.data)
      }
      break;
    default:
      console.log('elmdata', elmdata)
      throw new Error('unexpected input from Elm')
      break;
  }
})
