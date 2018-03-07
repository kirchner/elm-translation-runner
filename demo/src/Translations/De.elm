module Translations.De exposing (..)

import Translation exposing (..)
import Translation.De exposing (..)


applicationName : Translation args node
applicationName =
    final "applicationName" <|
        s "Yet Another Email Client"
