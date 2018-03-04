"use strict";

const compile = require("node-elm-compiler").compile;
const fs = require("fs");
const path = require("path");
const mkdirp = require("mkdirp");



var generateConfig = function(argv) {
  if (fs.existsSync("./elm-translation.json")) {
    console.log("You already have a configuration file.");
    process.exit(1);
  }

  var translationsDirectory = argv["translations-directory"];

  if (!fs.existsSync(translationsDirectory)) {
    console.log("The directory '" + translationsDirectory + "' does not exist.");
    process.exit(1);
  }

  var translationFiles = [];

  var files = fs.readdirSync(translationsDirectory);

  files.forEach(function(file) {
    var match = /^([^\.]\w*)\.json$/.exec(file)
    if (match) {
      var locale = match[1];
      
      translationFiles.push({
        "path": [ translationsDirectory, file ].join(path.sep),
        "locale": locale
      });
    }
  });

  var config = {
    "source-directory": argv["source-directory"],
    "module-prefix": argv["module-prefix"],
    "translation-files": translationFiles
  };

  fs.writeFile("./elm-translation.json", JSON.stringify(config, null, 4), function(error) {
    if (error) {
      console.log(error);
      process.exit(1);
    } else {
      console.log("Successfully generated configuration!");
    }
  });
};



var generateElm = function() {
  fs.readFile("./elm-translation.json", function(error, data) {
    if (error) {
      console.log("There was a problem while reading the configuration:", error);
      process.exit(1);
    }

    var config = JSON.parse(data)

    //compile(["src/Main.elm"], { output: "main.js" })
    //  .on("close", function(exitCode) {
        var Elm = require("./main.js");

        var locales = [];

        config["translation-files"].forEach(function({ path, locale }) {
          var rawJson =
            fs
              .readFileSync(path)
              .toString();

          locales.push({
            "locale": locale,
            "fileName": path,
            "rawJson": rawJson
          });
        });

        var worker = Elm.Main.worker({
          "locales": locales,
          "modulePrefix": config["module-prefix"]
        })

        worker.ports.writeModule.subscribe(function(data) {
          var scope = data["scope"];
          var content = data["content"];

          var modulePath =
            [ config["source-directory"] ]
              .concat(scope)
              .join(path.sep) + ".elm";

          console.log(modulePath);

          mkdirp(path.dirname(modulePath), function(error) {
            if (error) {
              console.log("could not create directories");
            } else {
              fs.writeFileSync(
                [ config["source-directory"] ]
                  .concat(scope)
                  .join(path.sep) + ".elm",
                content
              );
            }
          });
        });

        worker.ports.reportError.subscribe(function(data) {
          var errorMsg = data["error"];

          console.log(errorMsg);

          process.exit(1);
        });
      //});
  });
};


const argv =
  require ("yargs")
    .command(
      "init",
      "create a initial configuration in elm-translation.json",
      function(yargs) {
        yargs
          .option("translations-directory", {
            describe: "under which directory should we look for translation files",
            default: "./translations"
          })
          .option("module-prefix", {
            describe: "the module under which all translations live",
            default: "Translations"
          })
          .option("source-directory", {
            decribe: "where should generated modules be saved",
            default: "./src"
          })
      },
      function(argv) {
        generateConfig(argv);
      }
    )
    .command(
      "generate-elm",
      "convert JSON into Elm Translation modules",
      function() {},
      function(argv) {
        generateElm();
      }
    )
    .command(
      "generate-json",
      "convert Elm Translation modules into JSON"
    )
    .argv;
