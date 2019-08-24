const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const merge = require("webpack-merge");


const baseConfig = {
  // "production" or "development" flag.
  mode: "development",

  // Set the base directory manually, instead of CWD.
  context: path.join(__dirname, "src"),

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "app"),
    filename: "[name].js"
  },

};


const mainConfig = merge(baseConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-main",

  // Entry points into the code. The roots of the dependency tree.
  entry: {
    electron: "./main.js"
  },

  // Without this, __dirname refers to dirname of input file.
  // With this, refers to dirname of output file.
  // https://github.com/electron/electron/issues/5107
  node: {
    __dirname: false
  },

  // TODO : Understand what this is doing.
  externals: {
    "pouchdb": "require('pouchdb')",
    "7zip-bin": "require('7zip-bin')",
  }
});


const rendererConfig = merge(baseConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-renderer",

  // Entry points into the code. The root of the dependency tree.
  entry: {
    home: "./shared/home.js",
    doc: "./shared/doc.js"
  },

  // What file types to attempt to resolve within require/import statements
  resolve: {
    extensions: [".js", ".elm"]
  },

  // Rules on how to handle specific files.
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {verbose: true, pathToElm: "./elm-log-colors.sh"}
        }
      },
      {
        test: require.resolve("textarea-autosize"),
        use: {
          loader: "imports-loader",
          options: {jQuery : "jquery"}
        }
      }
    ]
  },

  externals: {
    "pouchdb": "require('pouchdb')",
  },

  plugins: [

    // Plugin to insert only needed chunks of JS into HTML output files.
    new HtmlWebpackPlugin({
      template: "./index.ejs",
      filename: "../app/static/index.html",
      chunks: ["doc"]
    }),
    new HtmlWebpackPlugin({
      template: "./home.ejs",
      filename: "../app/static/home.html",
      chunks: ["home"]
    }),

    // Plugin to copy static assets (css, images).
    new CopyWebpackPlugin([{
      from: "./static",
      to: "../app/static"
    }])
  ]
});


module.exports = [ mainConfig, rendererConfig ];

/*
  Unused configuration from Webpack 2
  ====

  externals : {
    "pouchdb-load": "require('pouchdb-load')",
    "pouchdb-promise": "require('pouchdb-promise')",
    "pouchdb-adapter-memory": "require('pouchdb-adapter-memory')",
    "electron-spellchecker": "require('electron-spellchecker')",
    "electron-store": "require('electron-store')",
    "dblsqd-electron": "require('dblsqd-electron')",
  },
*/
