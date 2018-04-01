module Translations exposing (..)

import Locales exposing (..)
import Text exposing (Static, Text)
import Translations.En as En exposing (..)
import Translations.De as De exposing (..)


applicationName : Locale -> Text Static args node
applicationName locale =
    case locale of
        En ->
            En.applicationName

        De ->
            De.applicationName
