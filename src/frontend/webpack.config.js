// Sets variables in "./env" on "process.env" global variable
require("dotenv").config();

const path = require("path");
const NodePolyfillPlugin = require("node-polyfill-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const webpack = require("webpack");

const isProduction = process.env.NODE_ENV == "production";

const config = {
  experiments: {
    asyncWebAssembly: true,
  },
  entry: "./index.js",
  output: {
    path: path.resolve(__dirname, "../backend/src/assets/frontend-dist"),
  },
  plugins: [
    new webpack.DefinePlugin({
      "process.env": {
        PAB_PROXY_URL: JSON.stringify(process.env.PAB_PROXY_URL),
      },
    }),
    // allows to use modules from NodeJS like `Buffer` or `Util`.
    new NodePolyfillPlugin(),
    // Creates separate CSS files from CSS imports in JS files.
    new MiniCssExtractPlugin(),
  ],
  module: {
    rules: [
      {
        test: /\.(js|jsx)$/i,
        loader: "babel-loader",
      },
      {
        test: /\.css$/i,
        use: [MiniCssExtractPlugin.loader, "css-loader"],
      },
      {
        test: /\.(eot|svg|ttf|woff|woff2|png|jpg|gif)$/i,
        type: "asset",
      },

      // Add your rules for custom modules here
      // Learn more about loaders from https://webpack.js.org/loaders/
    ],
  },
  resolve: {
    modules: ["node_modules", "lib"],
    // Used to prevent the error:
    // Module not found: Error: Can't resolve 'fs' in '/home/kolam/git/iog/plutus-starter/demo/pab-nami/client/node_modules/secrets/src'
    fallback: {
      fs: false,
    },
  },
  resolveLoader: {
    modules: ["node_modules", path.resolve(__dirname, ".")],
  },
};

module.exports = () => {
  if (isProduction) {
    config.mode = "production";
  } else {
    config.mode = "development";
  }
  return config;
};
