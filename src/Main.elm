module Main exposing (main)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform exposing (programWithFlags)
import Ports
import Set exposing (Set)
import String.Extra as String
import Translation exposing (ArgType(..))


type alias Flags =
    { modulePrefix : String
    , locales : List { locale : String, rawJson : String }
    }


main : Program Flags {} msg
main =
    programWithFlags
        { init = init
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


init : Flags -> ( {}, Cmd msg )
init { modulePrefix, locales } =
    ( {}
    , [ locales
            |> List.map (generateLocaleModules modulePrefix)
            |> Cmd.batch
      , case collect locales of
            Ok byScope ->
                [ byScope
                    |> Dict.map
                        (generateTranslationModule modulePrefix
                            (allLocales byScope)
                        )
                    |> Dict.values
                    |> Cmd.batch
                , byScope
                    |> allLocales
                    |> generateLocalesModule
                ]
                    |> Cmd.batch

            Err error ->
                Cmd.none
      ]
        |> Cmd.batch
    )



---- COLLECT MESSAGES


type alias Scope =
    List String


type alias Locale =
    String


type alias Key =
    String


type Directory
    = Directory (Dict Key Directory)
    | Entry String


allLocales : Dict Scope (Dict Key (Dict Locale String)) -> Set String
allLocales translations =
    translations
        |> Dict.values
        |> List.foldl
            (\byKey locales ->
                byKey
                    |> Dict.values
                    |> List.foldl
                        (\byLocale locales ->
                            byLocale
                                |> Dict.keys
                                |> List.foldl Set.insert locales
                        )
                        locales
            )
            Set.empty


collect :
    List { locale : String, rawJson : String }
    -> Result String (Dict Scope (Dict Key (Dict Locale String)))
collect rawData =
    rawData
        |> List.map
            (\{ locale, rawJson } ->
                case Decode.decodeString directoryDecoder rawJson of
                    Ok (Directory entries) ->
                        Ok
                            ( locale
                            , collectMessages entries
                            )

                    Ok _ ->
                        Err <|
                            "The locale '"
                                ++ locale
                                ++ "' does not contain any translations."

                    Err error ->
                        Err <|
                            "There was an error while parsing the translations for '"
                                ++ locale
                                ++ "':"
                                ++ error
            )
        |> List.foldl
            (\result listResult ->
                case listResult of
                    Ok list ->
                        case result of
                            Ok data ->
                                Ok (data :: list)

                            Err error ->
                                Err error

                    Err error ->
                        listResult
            )
            (Ok [])
        |> Result.map collectTranslations


directoryDecoder : Decoder Directory
directoryDecoder =
    Decode.oneOf
        [ Decode.dict (Decode.lazy (\_ -> directoryDecoder))
            |> Decode.map Directory
        , Decode.string
            |> Decode.map Entry
        ]


collectMessages : Dict String Directory -> Dict Scope (Dict Key String)
collectMessages entries =
    entries
        |> Dict.map (collectMessagesHelp [])
        |> Dict.values
        |> List.foldr merge Dict.empty


collectMessagesHelp :
    List String
    -> String
    -> Directory
    -> Dict (List String) (Dict String String)
collectMessagesHelp scope name directory =
    case directory of
        Entry newMessage ->
            Dict.singleton (List.reverse scope) (Dict.singleton name newMessage)

        Directory entries ->
            entries
                |> Dict.map (collectMessagesHelp (name :: scope))
                |> Dict.values
                |> List.foldr merge Dict.empty


collectTranslations :
    List ( Locale, Dict Scope (Dict Key String) )
    -> Dict Scope (Dict Key (Dict Locale String))
collectTranslations translations =
    translations
        |> List.foldl
            (\( locale, translationsByScope ) collected ->
                translationsByScope
                    |> Dict.toList
                    |> List.foldl
                        (\( scope, translations ) collected ->
                            let
                                newTranslationsByKeyAndLocale =
                                    translations
                                        |> Dict.map (\_ -> Dict.singleton locale)
                            in
                            collected
                                |> Dict.update scope
                                    (Maybe.withDefault Dict.empty
                                        >> merge newTranslationsByKeyAndLocale
                                        >> Just
                                    )
                        )
                        collected
            )
            Dict.empty



---- GENERATE LOCALES MODULE


generateLocalesModule : Set String -> Cmd msg
generateLocalesModule locales =
    [ ( "scope"
      , [ Encode.string "Locales" ]
            |> Encode.list
      )
    , ( "content"
      , [ "module Locales exposing (..)"
        , [ "type Locale"
          , [ "= "
            , locales
                |> Set.toList
                |> List.map String.toSentenceCase
                |> String.join "\n| "
            ]
                |> String.concat
                |> indent
          ]
            |> String.join "\n"
        ]
            |> String.join "\n\n\n"
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.writeModule



---- GENERATE TRANSLATION MODULES


generateTranslationModules :
    String
    -> Set String
    -> Dict Scope (Dict Key (Dict Locale String))
    -> Cmd msg
generateTranslationModules modulePrefix locales byScope =
    byScope
        |> Dict.map (generateTranslationModule modulePrefix locales)
        |> Dict.values
        |> Cmd.batch


generateTranslationModule :
    String
    -> Set String
    -> Scope
    -> Dict Key (Dict Locale String)
    -> Cmd msg
generateTranslationModule modulePrefix locales scope byKey =
    [ ( "scope"
      , (modulePrefix :: scope)
            |> List.map
                (String.toSentenceCase
                    >> String.camelize
                    >> Encode.string
                )
            |> Encode.list
      )
    , ( "content"
      , [ [ [ "module"
            , moduleName (modulePrefix :: scope)
            , "exposing (..)"
            ]
                |> String.join " "
          , [ "import Locales exposing (..)"
            , "import Translation exposing (Translation)"
            , locales
                |> Set.toList
                |> List.map
                    (\locale ->
                        [ "import"
                        , moduleName (modulePrefix :: scope ++ [ locale ])
                        , "as"
                        , String.toSentenceCase locale
                        , "exposing (..)"
                        ]
                            |> String.join " "
                    )
                |> String.join "\n"
            ]
                |> String.join "\n"
          ]
            |> String.join "\n\n"
        , byKey
            |> Dict.map (generateTranslationFunction locales)
            |> Dict.values
            |> String.join "\n\n\n"
        ]
            |> String.join "\n\n\n"
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.writeModule


generateTranslationFunction : Set String -> Key -> Dict Locale String -> String
generateTranslationFunction locales key byLocale =
    [ [ key
      , ": Locale ->"
      , byLocale
            |> Dict.values
            |> List.head
            |> Maybe.andThen
                (Translation.toElmType cldrToArgType)
            |> Maybe.withDefault "TODO: error"
      ]
        |> String.join " "
    , [ key
      , "locale ="
      ]
        |> String.join " "
    , [ "case locale of"
      , locales
            |> Set.toList
            |> List.map
                (\locale ->
                    [ String.toSentenceCase locale ++ " ->"
                    , [ String.toSentenceCase locale
                      , "."
                      , key
                      ]
                        |> String.concat
                        |> indent
                    ]
                        |> String.join "\n"
                )
            |> String.join "\n\n"
            |> indent
      ]
        |> String.join "\n"
        |> indent
    ]
        |> String.join "\n"



---- GENERATE LOCALE MODULES


generateLocaleModules : String -> { locale : String, rawJson : String } -> Cmd msg
generateLocaleModules modulePrefix { locale, rawJson } =
    case Decode.decodeString directoryDecoder rawJson of
        Ok (Directory entries) ->
            entries
                |> collectMessages
                |> Dict.map
                    (\scope messages ->
                        [ ( "scope"
                          , (modulePrefix :: (scope ++ [ locale ]))
                                |> List.map
                                    (String.toSentenceCase
                                        >> String.camelize
                                        >> Encode.string
                                    )
                                |> Encode.list
                          )
                        , ( "content"
                          , generateModule (modulePrefix :: scope) locale messages
                                |> Encode.string
                          )
                        ]
                            |> Encode.object
                            |> Ports.writeModule
                    )
                |> Dict.values
                |> Cmd.batch

        Ok _ ->
            Debug.crash "no translations present"

        Err error ->
            Debug.crash error


generateModule : List String -> String -> Dict String String -> String
generateModule scope locale messages =
    [ [ [ "module"
        , moduleName (scope ++ [ locale ])
        , "exposing (..)"
        ]
            |> String.join " "
      , [ "import Translation exposing (..)"
        , "import Translation." ++ String.toSentenceCase locale ++ " exposing (..)"
        ]
            |> String.join "\n"
      ]
        |> String.join "\n\n"
    , messages
        |> Dict.toList
        |> List.filterMap (uncurry generateMessage)
        |> String.join "\n\n\n"
    ]
        |> String.join "\n\n\n"


generateMessage : String -> String -> Maybe String
generateMessage name icuMessage =
    Translation.toElm cldrToArgType
        (String.camelize name)
        icuMessage


moduleName : List String -> String
moduleName =
    List.map (String.toSentenceCase >> String.camelize)
        >> String.join "."



---- CONFIGURATION


cldrToArgType names =
    case names of
        "_" :: "delimited" :: otherNames ->
            Just (ArgDelimited otherNames)

        "_" :: "list" :: otherNames ->
            Just (ArgList otherNames)

        _ :: "number" :: otherNames ->
            (Just << ArgFloat) <|
                case otherNames of
                    [] ->
                        [ "decimal", "standard" ]

                    _ ->
                        "decimal" :: otherNames

        _ :: "date" :: otherNames ->
            Just (ArgDate otherNames)

        _ :: "time" :: otherNames ->
            Just (ArgTime otherNames)

        _ :: "plural" :: otherNames ->
            case otherNames of
                [] ->
                    Just (ArgCardinal [ "decimal", "standard" ])

                _ ->
                    Just (ArgCardinal otherNames)

        _ :: "selectordinal" :: otherNames ->
            case otherNames of
                [] ->
                    Just (ArgOrdinal [ "decimal", "standard" ])

                _ ->
                    Just (ArgOrdinal otherNames)

        _ :: "node" :: [] ->
            Just ArgNode

        _ :: [] ->
            Just ArgString

        _ ->
            Nothing



---- HELPER


{-| Merge two dictionaries of the form `Dict a (Dict b c)` by taking the union
on the inner dictionaries.
-}
merge :
    Dict comparable1 (Dict comparable2 a)
    -> Dict comparable1 (Dict comparable2 a)
    -> Dict comparable1 (Dict comparable2 a)
merge dictA dictB =
    Dict.merge
        Dict.insert
        (\scope a b collection -> Dict.insert scope (Dict.union a b) collection)
        Dict.insert
        dictA
        dictB
        Dict.empty


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
