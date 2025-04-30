const path = require('path');

module.exports = {
  mode: 'production',
  entry: './temp/worker.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'worker.js',
  },
  target: 'node',
};