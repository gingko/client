import { app } from 'electron'
import * as fs from 'fs/promises'
import path from 'path'
const util = require('util')
const execFile = util.promisify(require('child_process').execFile)

export default async function pandoc (targetPath, content) {
  const tmpMarkdown = path.join(app.getPath('temp'), path.basename(targetPath) + '.md')
  await fs.writeFile(tmpMarkdown, content)

  let pandocPath = path.join(__dirname, '/../pandoc')

  if (!app.isPackaged) {
    switch (process.platform) {
      case 'linux':
        pandocPath = path.join(__dirname, '/../src/bin/linux/pandoc')
        break

      case 'win32':
        pandocPath = path.join(__dirname, '/../src/bin/win/pandoc.exe')
        break

      case 'darwin':
        pandocPath = path.join(__dirname, '/../src/bin/mac/pandoc')
        break
    }
  }

  await execFile(pandocPath,
    [tmpMarkdown,
      '--from=gfm+hard_line_breaks',
      '--to=docx',
      `--output=${targetPath}`,
      '--verbose'
    ])

  await fs.unlink(tmpMarkdown)
}
