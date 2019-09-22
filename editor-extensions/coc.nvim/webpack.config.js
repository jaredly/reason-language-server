const path = require('path');

const config = {
  mode: 'production',
  target: 'node',
  entry: './src/index',
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: [{ loader: 'babel-loader' }],
      },
    ],
  },
  output: {
    path: path.join(__dirname, 'lib'),
    filename: 'index.js',
    libraryTarget: 'commonjs',
  },
};

module.exports = config;
