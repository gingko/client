var path = require('path');

module.exports = {
  entry: './src/web/main.js',
  output: {
    path: path.join(__dirname, 'dist'),
    publicPath: '/dist/',
    filename: 'bundle.js'
  }
}
