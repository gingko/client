import * as path from 'node:path';

const myAliasPlugin = {
  name: "AliasPlugin",
  setup(build) {
    build.onResolve({ filter: /^Container$/ }, () => {
      return { path: path.join(import.meta.dir, "./src/web/container-web.js") };
    });
  },
};

const result = await Bun.build({
  entrypoints: ['./src/shared/doc.js', './src/shared/data.worker.js'],
  outdir: './web',
  target: 'browser',
  plugins: [myAliasPlugin],
  define: {
    'global': 'self',
  },
  sourcemap: 'external',
  minify: true,
})

if (!result.success) {
  console.error("Build failed");
  for (const message of result.logs) {
    // Bun will pretty print the message object
    console.error(message);
  }
}