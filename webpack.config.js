const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const merge = require("webpack-merge");



/* ======= WEB TARGET ======= */

const webConfig = {
  // "production" or "development" flag.
  mode: "development",

  target: "web",

  // Set the base directory manually, instead of CWD.
  context: path.join(__dirname, "src"),

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "web"),
    filename: "[name].js"
  },

  // Entry points into the code. The root of the dependency tree.
  entry: {
    doc: "./shared/doc.js"
  },

  // What file types to attempt to resolve within require/import statements
  resolve: {
    alias: { Container: path.resolve(__dirname, "src/web/container.js") },
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

  /*
  externals: {
    "pouchdb": "require('pouchdb')",
  },
  */

  plugins: [

    // Plugin to insert only needed chunks of JS into HTML output files.
    new HtmlWebpackPlugin({
      template: "./index.ejs",
      filename: "../web/static/index.html",
      chunks: ["doc"]
    }),

    // Plugin to copy static assets (css, images).
    new CopyWebpackPlugin([{
      from: "./static",
      to: "../web/static"
    }])
  ]
};


/* ======= ELECTRON TARGET ======= */

const baseElectronConfig = {
  // "production" or "development" flag.
  mode: "development",

  devtool: "source-map",

  // Set the base directory manually, instead of CWD.
  context: path.join(__dirname, "src"),

  // Without this, __dirname refers to dirname of input file.
  // With this, refers to dirname of output file.
  // https://github.com/electron/electron/issues/5107
  node: {
    __dirname: false
  },

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "app"),
    filename: "[name].js"
  }
};


const mainConfig = merge(baseElectronConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-main",

  // Entry points into the code. The roots of the dependency tree.
  entry: {
    electron: "./electron/main.js"
  },

  // TODO : Understand what this is doing.
  externals: {
    "pouchdb": "require('pouchdb')",
    "pouchdb-load": "require('pouchdb-load')",
    "7zip-bin": "require('7zip-bin')",
  }
});


const rendererConfig = merge(baseElectronConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-renderer",

  // Entry points into the code. The root of the dependency tree.
  entry: {
    home: "./shared/home.js",
    doc: "./shared/doc.js"
  },

  // What file types to attempt to resolve within require/import statements
  resolve: {
    alias: { Container: path.resolve(__dirname, "src/electron/container.js") },
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


module.exports = [ webConfig, mainConfig, rendererConfig ];
