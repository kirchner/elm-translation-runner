module Translations.Feature exposing (..)

import Translations.Locales exposing (..)
import Translation exposing (Translation)
import Translations.Feature.De as De exposing (..)
import Translations.Feature.En as En exposing (..)


emailInfo : Locale -> Translation { args | count : Float } msg
emailInfo locale =
    case locale of
        De ->
            De.emailInfo
        
        En ->
            En.emailInfo


greeting : Locale -> Translation { args | name : String } msg
greeting locale =
    case locale of
        De ->
            De.greeting
        
        En ->
            En.greeting