var path = require('path');
var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  target: 'electron',
  context: path.join(__dirname, 'src'),
  entry: {
      electron: './electron-start.js',
      main: './shared/main.js',
      listwindow: './shared/listwindow.js'
  },
  output: {
    path: path.join(__dirname, 'dist'),
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
          options: {verbose: true, warn: true}
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
    pouchdb: "require('pouchdb')",
    "pouchdb-replication-stream": "require('pouchdb-replication-stream')",
    "pouchdb-promise": "require('pouchdb-promise')"
  },
  node: {
    __dirname: false
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './static/index.html',
      filename: './static/index.html',
      chunks: ['main']
    }),
    new HtmlWebpackPlugin({
      template: './static/list.html',
      filename: './static/list.html',
      chunks: ['listwindow'],
    })
  ]
}
