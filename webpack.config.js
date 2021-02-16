const path = require("path");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const CopyWebpackPlugin = require("copy-webpack-plugin");
const merge = require("webpack-merge");
const zh_hans = require("./i18n/zh_hans.json");
const zh_hant = require("./i18n/zh_hant.json");
const es = require("./i18n/es.json");
const fr = require("./i18n/fr.json");
const de = require("./i18n/de.json");
const nl = require("./i18n/nl.json");
const hu = require("./i18n/hu.json");
const sv = require("./i18n/sv.json");
const ca = require("./i18n/ca.json");
const br = require("./i18n/br.json");



/* ======= WEB TARGET ======= */

const prepTranslation = (langCode, langData) => {
  return langData.flatMap(t => {
    let target = t.reference.replace('Elm:', `%${langCode}:`);
    let replacement = t.definition;
    if (typeof target === "string" && target.startsWith(`%${langCode}`) && typeof replacement === "string" ) {
      // Regular replacement
      return [{ search: target
        , replace: replacement.replace(/'/g,"\\'")
        , flags : 'g'
      }];
  } else if (replacement === null) {
      return [{ search: target
        , replace: t.term.replace(/'/g, "\\'")
        , flags : 'g'
      }];
  } else if (t.hasOwnProperty("term_plural") && typeof replacement == "object" && replacement.hasOwnProperty("one")) {
      // Plural replacement
      let singReplace = replacement.one.replace(/'/g,"\\'");
      let plurReplace = replacement.other.replace(/'/g, "\\'");
      plurReplace = plurReplace == "" ? t.term_plural : plurReplace;
      return [
        { search: target+":0" , replace: singReplace, flags : 'g' },
        { search: target+":1" , replace: plurReplace, flags:  'g' }];
    } else {
      return [];
    }
  });
}

const zhHansT = prepTranslation("zh_hans", zh_hans);
const zhHantT = prepTranslation("zh_hant", zh_hant);
const esT = prepTranslation("es", es);
const frT = prepTranslation("fr", fr);
const deT = prepTranslation("de", de);
const nlT = prepTranslation("nl", nl);
const huT = prepTranslation("hu", hu);
const svT = prepTranslation("sv", sv);
const caT = prepTranslation("ca", ca);
const brT = prepTranslation("br", br);

const allLanguageStrings = [].concat(zhHansT, zhHantT, esT, frT, deT, nlT, huT, svT, caT, brT)

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
            options : { multiple : allLanguageStrings }
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
