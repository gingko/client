import { Elm } from '../elm/Home'

let homeApp

async function init (flags) {
  homeApp = Elm.Home.init({ flags })

  homeApp.ports.send.subscribe((tag) => {
    switch (tag) {
      case 'ClickedNew':
        window.electronAPI.clickedNew()
        break

      case 'ClickedOpen':
        window.electronAPI.clickedOpen()
        break
    }
  })
}

window.electronAPI.fileReceived(async (event, value) => {
  console.log('fileReceived in home.js', value)
  await init(value)
})
