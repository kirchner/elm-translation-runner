"use strict";

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

  var locales = [];

  var files = fs.readdirSync(translationsDirectory);

  files.forEach(function(file) {
    var match = /^([^\.]\w*)\.json$/.exec(file)
    if (match) {
      var locale = match[1];
      
      locales.push({
        "name": locale,
        "code": locale,
        "file-path": [ translationsDirectory, file ].join(path.sep),
        "fallbacks": []
      });
    }
  });

  var config = {
    "source-directory": argv["source-directory"],
    "module-prefix": argv["module-prefix"],
    "module-name": argv["module-name"],
    "locales": locales
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
      if (error["code"] == "ENOENT") {
        console.log("I could not find the configuration file elm-translation.json,")
        console.log("please run\n")
        console.log("  $ elm-translation init\n")
      } else {
        console.log("There was a problem while reading the configuration:", error);
      }
      process.exit(1);
    }

    var config = JSON.parse(data)

    var Elm = require("./main.js");

    var worker = Elm.Main.worker(config);

    worker.ports.fetchFile.subscribe(function(path) {
      var content =
        fs
          .readFileSync(path)
          .toString();

      worker.ports.fileReceived.send({
        "path": path,
        "content": content
      });
    });

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
  });
};


const argv =
  require ("yargs")
    .command(
      "init",
      "create an initial configuration file elm-translation.json",
      function(yargs) {
        yargs
          .option("translations-directory", {
            describe: "under which directory should we look for translation files",
            default: "./translations"
          })
          .option("module-name", {
            describe: "the name of the (top-level) translations module",
            default: "Translations"
          })
          .option("module-prefix", {
            describe: "the module under which the translations module should live",
            default: ""
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
    // TODO
    //.command(
    //  "generate-json",
    //  "convert Elm Translation modules into JSON"
    //)
    .argv;
