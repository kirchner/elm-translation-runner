module Translations.Feature exposing (..)

import Locales exposing (..)
import Translation exposing (Translation)
import Translations.Feature.En as En exposing (..)
import Translations.Feature.De as De exposing (..)


emailInfo : Locale -> Translation { args | count : Float } node
emailInfo locale =
    case locale of
        En ->
            En.emailInfo

        De ->
            De.emailInfo


greeting : Locale -> Translation { args | name : String } node
greeting locale =
    case locale of
        En ->
            En.greeting

        De ->
            De.greeting


missingInDe : Locale -> Translation args node
missingInDe locale =
    case locale of
        En ->
            En.missingInDe

        De ->
            De.missingInDe
