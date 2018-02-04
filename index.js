"use strict";

const compile = require("node-elm-compiler").compile;
const fs = require("fs");
const path = require("path");
const mkdirp = require("mkdirp");


var generateElm = function() {
  compile(["src/Main.elm"], { output: "main.js" })
    .on("close", function(exitCode) {
      var Elm = require("./main.js");

      var rawJson =
        fs
          .readFileSync("./translations/en.json")
          .toString();

      var worker = Elm.Main.worker({ rawJson: rawJson });

      worker.ports.writeModule.subscribe(function(data) {
        var scope = data["scope"];
        var content = data["content"];

        var modulePath = "generated/Translations/" + scope.join(path.sep) + ".elm";

        mkdirp(path.dirname(modulePath), function(error) {
          if (error) {
            console.log("could not create directories");
          } else {
            fs.writeFileSync(
              "./generated/Translations/" + scope.join(path.sep) + ".elm",
              content
            );
          }
        });
      });
    });
};


const argv =
  require ("yargs")
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
