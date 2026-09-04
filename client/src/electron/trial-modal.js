import { Elm } from '../elm/Electron/TrialModal'

const args = window.electronAPI.getArgs()

const trialModal = Elm.Electron.TrialModal.init({ flags: args })

trialModal.ports.clicked.subscribe((tag) => {
  switch (tag) {
    case 'enter':
      window.electronAPI.enterSerialClicked()
      break

    case 'continue':
      window.electronAPI.continueClicked()
      break
  }
})
