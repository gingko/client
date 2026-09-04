import * as esbuild from 'esbuild';
import path from 'node:path'

const myAliasPlugin = {
  name: "AliasPlugin",
  setup(build) {
    build.onResolve({ filter: /^Container$/ }, () => {
      return { path: path.join(import.meta.dir, "./src/web/container-web.js") };
    });
  },
};

const result = await esbuild.build({
  entryPoints: ['./src/shared/doc.js', './src/shared/data.worker.js'],
  outdir: './web',
  plugins: [myAliasPlugin],
  minify: true,
  platform: 'browser',
  bundle: true,
  external: ['crypto'],
  define: {
    'global': 'self',
  },
});

if (result.errors.length > 0) {
  console.error("Build failed");
  console.error(result.errors);
} else {
  console.log("\x1b[32m%s\x1b[0m", "Build succeeded");
}
