module Translations.Feature exposing (..)

import Locales exposing (..)
import Translation exposing (Translation)
import Translations.Feature.De as De exposing (..)
import Translations.Feature.En as En exposing (..)


emailInfo : Locale -> Translation { args | count : Float } node
emailInfo locale =
    case locale of
        De ->
            De.emailInfo

        En ->
            En.emailInfo


greeting : Locale -> Translation { args | name : String } node
greeting locale =
    case locale of
        De ->
            De.greeting

        En ->
            En.greeting


missingInDe : Locale -> Translation args node
missingInDe locale =
    case locale of
        De ->
            De.missingInDe

        En ->
            En.missingInDe
