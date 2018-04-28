const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');


module.exports = {
  target: 'electron',
  context: path.join(__dirname, 'src'),
  entry: {
      electron: './electron-start.js',
      main: './shared/main.js'
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
    "electron-store": "require('electron-store')",
    "dblsqd-electron": "require('dblsqd-electron')",
    "pouchdb-adapter-memory": "require('pouchdb-adapter-memory')",
    "pouchdb-replication-stream": "require('pouchdb-replication-stream')",
    "pouchdb-promise": "require('pouchdb-promise')"
  },
  node: {
    __dirname: false
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './index.ejs',
      filename: '../app/static/index.html',
      chunks: ['main']
    })
  , new CopyWebpackPlugin([{
      from: './static',
      to: '../app/static'
    }])
  , new CopyWebpackPlugin([{
      from: `./bin/${process.platform}`,
      to: `../app/static/bin`
    }])
  ]
}
