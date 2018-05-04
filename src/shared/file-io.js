const fs = require('fs')

function dbToFile(database, filepath) {
  return new Promise((resolve, reject) => {
    let ws = fs.createWriteStream(filepath)
    ws.on('error', reject)

    database.dump(ws)
      .then(() => {resolve(filepath)})
      .catch(reject)
  })
}


module.exports =
  { dbToFile: dbToFile
  }
