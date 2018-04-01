module Translations.Feature exposing (..)

import Locales exposing (..)
import Text exposing (Static, Text)
import Translations.Feature.En as En exposing (..)
import Translations.Feature.De as De exposing (..)


documentationInfo : Locale -> Text Static { args | documentation : List node -> node } node
documentationInfo locale =
    case locale of
        En ->
            En.documentationInfo

        De ->
            De.documentationInfo


emailInfo : Locale -> Text Static { args | count : Float } node
emailInfo locale =
    case locale of
        En ->
            En.emailInfo

        De ->
            De.emailInfo


greeting : Locale -> Text Static { args | name : String } node
greeting locale =
    case locale of
        En ->
            En.greeting

        De ->
            De.greeting


missingInDe : Locale -> Text Static args node
missingInDe locale =
    case locale of
        En ->
            En.missingInDe

        De ->
            De.missingInDe


quotation : Locale -> Text Static args node
quotation locale =
    case locale of
        En ->
            En.quotation

        De ->
            De.quotation
