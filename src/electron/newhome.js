import { Elm } from "../elm/Home";


const homeApp = Elm.Home.init();

homeApp.ports.send.subscribe((tag) => {
  switch (tag) {
    case "ClickedNew":
      window.electronAPI.clickedNew()
      break;

    case "ClickedOpen":
      window.electronAPI.clickedOpen()
      break;
  }
})