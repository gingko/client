const {ipcRenderer, dialog, remote} = require("electron");
import {Elm} from "../elm/Page/HomeElectron";
const dbMapping = require("../electron/doc-list");
const Store = require("electron-store");


ipcRenderer.on("menu:contact-support", () => { shell.openExternal("mailto:adriano@gingkoapp.com"); });

ipcRenderer.on("menu:language-select", (event, data) => {
  userStore.set("language", data);
  ipcRenderer.send("doc:language-changed", data);
  if (typeof data === "string") {
    home.ports.languageChanged.send(data);
  }
});

ipcRenderer.on("main:doc-list-reload", () => {
  let docList = dbMapping.getDocList();
  home.ports.docListReload.send(docList);
});


let docList = dbMapping.getDocList();
var homeWindow = remote.getCurrentWindow();
const userStore = new Store({name: "config"});
var home = Elm.Home.init({node: document.getElementById("elm"), flags: [Date.now(), docList, userStore.get("language") || "en"]});

home.ports.forJS.subscribe(function(elmdata) {
  switch(elmdata.tag){
    case "New":
      ipcRenderer.send("home:new");
      break;
    case "ImportGko":
      ipcRenderer.send("home:import-file");
      break;
    case "Open":
      ipcRenderer.send("home:open", elmdata.data[0], elmdata.data[1]);
      break;
    case "OpenOther":
      ipcRenderer.send("home:open-other");
      break;
    case "SetState":
      dbMapping.setState(elmdata.data[0], elmdata.data[1]);
      break;
    case "Delete":
      if(confirm("Are you sure you want to delete this?\nThere is NO UNDO.")) {
        ipcRenderer.send("home:delete", elmdata.data);
      }
      break;
    default:
      console.log("elmdata", elmdata);
      throw new Error("unexpected input from Elm");
      break;
  }
});
