var path = require('path');

module.exports = {
  entry: './src/web/main.js',
  module: {
    loaders: [
      {
        test: require.resolve("textarea-autosize"),
        loader: "imports-loader?jQuery=jquery"
      }
    ]
  },
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/dist/',
    filename: 'bundle.js'
  }
}
