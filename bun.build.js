import * as path from 'node:path';
import { watch } from "fs";
console.log('Building...')

const myAliasPlugin = {
  name: "AliasPlugin",
  setup(build) {
    build.onResolve({ filter: /^Container$/ }, () => {
      return { path: path.join(import.meta.dir, "./src/web/container-web.js") };
    });
  },
};

const watcher = watch(`${import.meta.dir}/src`,
  { recursive: true },
  async (event, filename) => {
  console.log(`Detected ${event} in ${filename}`);
  if (!filename.endsWith('.js')) return;

  console.log('Rebuilding...');
  const result = await Bun.build({
    entrypoints: ['./src/shared/doc.js'],
    outdir: './web',
    target: 'browser',
    plugins: [myAliasPlugin],
    define: {
      'global': 'window',
    },
    sourcemap: 'external',
  })

  if (!result.success) {
    console.error("Build failed");
    for (const message of result.logs) {
      // Bun will pretty print the message object
      console.error(message);
    }
  }
});

process.on("SIGINT", () => {
  watcher.close();
  process.exit(0);
});
