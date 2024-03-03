const path = require('path');

/* eslint-disable no-undef */
module.exports = {
    mode: 'development',
    entry:  __dirname + "/src/esprima.js",
    output: {
        path:  path.resolve(__dirname + "/dist/commonjs"),
        filename: "index.js",
        libraryTarget: "umd",
        library: "esprima"
    }
}
