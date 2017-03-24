var path = require('path');
var platform;

module.exports = {
  entry: `./src/${process.env.TARGET}/main.js`,
  target: process.env.TARGET,
  module: {
    loaders: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'elm-webpack-loader?verbose=true&warn=true',
      },
      {
        test: require.resolve("textarea-autosize"),
        loader: "imports-loader?jQuery=jquery"
      }
    ]
  },
  resolve: {
    extensions: ['.js', '.elm']
  },
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/dist/',
    filename: 'bundle.js'
  }
}
