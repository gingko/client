const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');


module.exports = {
  target: 'electron',
  context: path.join(__dirname, 'src'),
  entry: {
      electron: './main.js',
      home: './shared/home.js',
      doc: './shared/doc.js'
  },
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
          options: {verbose: true, warn: true, pathToMake: './elm-log-colors.sh'}
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
    "font-scanner": "require('font-scanner')",
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
