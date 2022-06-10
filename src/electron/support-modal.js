import { Elm } from '../elm/SupportModal'
const { clipboard, ipcRenderer } = require('electron')
const config = require('../../config.js')

const supportModal = Elm.SupportModal.init()

supportModal.ports.copyEmail.subscribe((isUrgent) => {
  clipboard.writeText(isUrgent ? config.SUPPORT_URGENT_EMAIL : config.SUPPORT_EMAIL)
})

supportModal.ports.submitForm.subscribe(async (formData) => {
  console.log(formData)
  const res = await window.fetch(config.PRODUCTION_SERVER + '/pleasenospam', { method: 'POST', body: JSON.stringify(formData), headers: { 'Content-Type': 'application/json' } })
  if (res.ok) {
    ipcRenderer.send('close-window')
  } else {
    window.alert('Could not send for some reason.\nTry again, or copy the email and send it manually.')
  }
})
