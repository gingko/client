import { Elm } from "../elm/Home";


const homeApp = Elm.Home.init();

homeApp.ports.send.subscribe(() => {
  window.electronAPI.clickedNew()
})