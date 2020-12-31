const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const merge = require("webpack-merge");
const es = require("./i18n/es.json");
const zh = require("./i18n/zh.json");



/* ======= WEB TARGET ======= */

const prepTranslation = (langCode, langData) => {
  return langData.flatMap(t => {
    let target = t.reference.replace('Elm:', `%${langCode}:`);
    let replacement = typeof t.definition === "string" ? t.definition : t.term;
    if (typeof target === "string" && target.startsWith(`%${langCode}`) && typeof replacement === "string" ) {
      return [{ search: target
        , replace: replacement
        , flags : 'g'
      }];
    } else {
      return [];
    }
  });
}

const esTranslations = prepTranslation("es", es);
const zhTranslations = prepTranslation("zh_hans", zh);

const webConfig = {
  // "production" or "development" flag.
  mode: "production",

  target: "web",

  // Set the base directory manually, instead of CWD.
  context: path.join(__dirname, "src"),

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "web"),
    publicPath: "/",
    filename: "[name].js"
  },

  // Entry points into the code. The root of the dependency tree.
  entry: {
    doc: "./shared/doc.js"
  },

  stats: 'errors-only',

  // What file types to attempt to resolve within require/import statements
  resolve: {
    alias: { Container: path.resolve(__dirname, "src/web/container.js") },
    extensions: [".js", ".elm"]
  },

  // Rules on how to handle specific files.
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: "string-replace-loader",
            options : { multiple : [].concat(esTranslations, zhTranslations) }
          },
          {
            loader: "elm-webpack-loader",
            options: {optimize: true, verbose: true, pathToElm: "./elm-log-colors.sh"}
          }
        ]
      },
      {
        test: require.resolve("textarea-autosize"),
        use: {
          loader: "imports-loader",
          options: {jQuery : "jquery"}
        }
      }
    ]
  },

  /*
  externals: {
    "pouchdb": "require('pouchdb')",
  },
  */

  plugins: [

    // Plugin to insert only needed chunks of JS into HTML output files.
    new HtmlWebpackPlugin({
      template: "./index.ejs",
      filename: "../web/index.html",
      chunks: ["doc"]
    }),

    // Plugin to copy static assets (css, images).
    new CopyWebpackPlugin({ patterns: [{
      from: "./static",
      to: "../web"
    }]})
  ]
};


/* ======= ELECTRON TARGET ======= */

const baseElectronConfig = {
  // "production" or "development" flag.
  mode: "development",

  devtool: "source-map",

  // Set the base directory manually, instead of CWD.
  context: path.join(__dirname, "src"),

  // Without this, __dirname refers to dirname of input file.
  // With this, refers to dirname of output file.
  // https://github.com/electron/electron/issues/5107
  node: {
    __dirname: false
  },

  stats: 'errors-only',

  // Where to output bundled code.
  output: {
    path: path.join(__dirname, "app"),
    filename: "[name].js"
  }
};


const mainConfig = merge(baseElectronConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-main",

  // Entry points into the code. The roots of the dependency tree.
  entry: {
    electron: "./electron/main.js"
  },

  // TODO : Understand what this is doing.
  externals: {
    "pouchdb": "require('pouchdb')",
    "pouchdb-load": "require('pouchdb-load')",
    "7zip-bin": "require('7zip-bin')",
  }
});


const rendererConfig = merge(baseElectronConfig, {
  // Set the target environment where the code bundles will run.
  target: "electron-renderer",

  // Entry points into the code. The root of the dependency tree.
  entry: {
    home: "./shared/home.js",
    doc: "./shared/doc.js"
  },

  // What file types to attempt to resolve within require/import statements
  resolve: {
    alias: { Container: path.resolve(__dirname, "src/electron/container.js") },
    extensions: [".js", ".elm"]
  },

  // Rules on how to handle specific files.
  module: {
    rules: [
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: {
          loader: "elm-webpack-loader",
          options: {verbose: true, pathToElm: "./elm-log-colors.sh"}
        }
      },
      {
        test: require.resolve("textarea-autosize"),
        use: {
          loader: "imports-loader",
          options: {jQuery : "jquery"}
        }
      }
    ]
  },

  externals: {
    "pouchdb": "require('pouchdb')",
  },

  plugins: [

    // Plugin to insert only needed chunks of JS into HTML output files.
    new HtmlWebpackPlugin({
      template: "./index.ejs",
      filename: "../app/static/index.html",
      chunks: ["doc"]
    }),
    new HtmlWebpackPlugin({
      template: "./home.ejs",
      filename: "../app/static/home.html",
      chunks: ["home"]
    }),

    // Plugin to copy static assets (css, images).
    new CopyWebpackPlugin({ patterns: [{
      from: "./static",
      to: "../app/static"
    }]})
  ]
});


module.exports = [ webConfig, mainConfig, rendererConfig ];
