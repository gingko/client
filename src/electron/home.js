import { Elm } from '../elm/Home'

let homeApp

async function init (flags) {
  homeApp = Elm.Home.init({ flags })

  homeApp.ports.send.subscribe(([tag, data]) => {
    switch (tag) {
      case 'ClickedNew':
        window.electronAPI.clickedNew()
        break

      case 'ClickedOpen':
        window.electronAPI.clickedOpen()
        break

      case 'ClickedImport':
        window.electronAPI.clickedImport()
        break

      case 'ClickedDocument':
        window.electronAPI.clickedDocument(data)
        break

      case 'ClickedRemoveDocument':
        window.electronAPI.clickedRemoveDocument(data)
        break
    }
  })
}

window.electronAPI.fileReceived(async (event, value) => {
  value.currentTime = Date.now()
  await init(value)
})
