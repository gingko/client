import { Elm } from "../elm/Home";


console.log("Hello from NEW HOME!", Elm)


const homeApp = Elm.Home.init();

homeApp.ports.send.subscribe(() => {
  console.log("CLICKED!")
  window.electronAPI.clickedNew()
})