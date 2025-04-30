const path = require('path');

module.exports = {
  entry: './temp/worker.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'worker.js',
  },
  target: 'node',
};