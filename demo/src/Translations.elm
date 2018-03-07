module Translations exposing (..)

import Locales exposing (..)
import Translation exposing (Translation)
import Translations.En as En exposing (..)
import Translations.De as De exposing (..)


applicationName : Locale -> Translation args node
applicationName locale =
    case locale of
        En ->
            En.applicationName

        De ->
            De.applicationName
