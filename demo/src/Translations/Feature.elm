module Translations.Feature exposing (..)

import Locales exposing (..)
import Translation exposing (Translation)
import Translations.Feature.En as En exposing (..)
import Translations.Feature.De as De exposing (..)


documentationInfo : Locale -> Translation { args | documentation : List node -> node } node
documentationInfo locale =
    case locale of
        En ->
            En.documentationInfo

        De ->
            De.documentationInfo


emailInfo : Locale -> Translation args node
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


teamInfo : Locale -> Translation args node
teamInfo locale =
    case locale of
        En ->
            En.teamInfo

        De ->
            De.teamInfo
