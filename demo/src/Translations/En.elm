module Translations.En exposing (..)

import Translation exposing (..)
import Translation.En exposing (..)


applicationName : Translation args node
applicationName =
    fallback "applicationName" <|
        s "Yet Another Email Client"
