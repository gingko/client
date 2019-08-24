const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");


const mainConfig = {
  // Set the target environment where the code bundles will run.
  target: "electron-main",

  // "production" or "development" flag. TODO : shared config
  mode: "development",

  // Set the base directory manually, instead of CWD. TODO : shared config
  context: path.join(__dirname, "src"),

  // Entry points into the code. The roots of the dependency tree.
  entry: {
    electron: "./main.js"
  },

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "app"),
    filename: "[name].js"
  },

  // Without this, __dirname refers to dirname of input file.
  // With this, refers to dirname of output file.
  // https://github.com/electron/electron/issues/5107
  node: {
    __dirname: false
  },


  // TODO : Understand what this is doing.
  externals: {
    "pouchdb": "require('pouchdb')"
  },

};


const rendererConfig = {
  // Set the target environment where the code bundles will run.
  target: "electron-renderer",

  // "production" or "development" flag. TODO : shared config
  mode: "development",

  // Set the base directory manually, instead of CWD. TODO : shared config
  context: path.join(__dirname, "src"),

  // Entry points into the code. The root of the dependency tree.
  entry: {
    home: "./shared/home.js" //,
    //doc: "./shared/doc.js"
  },

  // Where to output bundled code. TODO : shared config
  output: {
    path: path.join(__dirname, "app"),
    filename: "[name].js"
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
      }
    ]
  },

  plugins: [

    // Plugin to insert only needed chunks of JS into HTML output files.
    new HtmlWebpackPlugin({
      template: "./home.ejs",
      filename: "../app/static/home.html",
      chunks: ["home"]
    })

    // Plugin to copy static assets (css, images).
  , new CopyWebpackPlugin([{
      from: "./static",
      to: "../app/static"
    }])
  ]
};


module.exports = [ mainConfig, rendererConfig ];

/* module.exports = {
  target: 'electron-main',
  context: path.join(__dirname, 'src'),
  entry: {
      electron: './main.js',
      home: './shared/home.js',
      doc: './shared/doc.js'
  },
  mode: 'development',
  output: {
    path: path.join(__dirname, 'app'),
    filename: '[name].js'
  },
  resolve: {
    extensions: ['.js', '.elm']
  },
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: 'elm-webpack-loader',
          options: {verbose: true, pathToElm: './elm-log-colors.sh'}
        }
      },
      {
        test: require.resolve("textarea-autosize"),
        use: {
          loader: 'imports-loader',
          options: {jQuery : 'jquery'}
        }
      }
    ]
  },
  externals : {
    "7zip-bin": "require('7zip-bin')",
    "electron-spellchecker": "require('electron-spellchecker')",
    "electron-store": "require('electron-store')",
    "dblsqd-electron": "require('dblsqd-electron')",
    "pouchdb": "require('pouchdb')",
    "pouchdb-adapter-memory": "require('pouchdb-adapter-memory')",
    "pouchdb-load": "require('pouchdb-load')",
    "pouchdb-promise": "require('pouchdb-promise')"
  },
  node: {
    __dirname: false
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './index.ejs',
      filename: '../app/static/index.html',
      chunks: ['doc']
    })
  , new HtmlWebpackPlugin({
      template: './home.ejs',
      filename: '../app/static/home.html',
      chunks: ['home']
    })
  , new CopyWebpackPlugin([{
      from: './static',
      to: '../app/static'
    }])
  ]
}
*/
