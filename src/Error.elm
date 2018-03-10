module Error
    exposing
        ( Error(..)
        , epilog
        , mapDictWithErrors
        , mapWithErrors
        , print
        )

import Char
import Dict exposing (Dict)
import List.Extra as List
import Parser
import String.Extra as String
import Translation


type Error
    = Errors (List Error)
    | Unexpected String
    | InvalidLocaleNames (List String)
    | MissingTranslation
        { localeName : String
        , filePath : String
        , availableFallbackLocales : List String
        , scopeKey : ( List String, String )
        }
    | IcuErrors
        { localeName : String
        , filePath : String
        , scopeKey : ( List String, String )
        , icuErrors : List Translation.Error
        }
    | JSONSyntax
        { localeName : String
        , filePath : String
        , errorMsg : String
        }


epilog : String
epilog =
    [ "Were the above error messages helpful? If not, feel free to open an issue on "
    , yellowText "github.com/kirchner/elm-translation-runner"
    , " including the error and a short explanation why they did not help you!"
    ]
        |> String.concat
        |> String.softWrap 80


print : Error -> String
print error =
    case error of
        Errors errors ->
            errors
                |> List.map print
                |> String.join "\n\n"

        Unexpected message ->
            [ title "unexpected error" ""
            , String.softWrap 80 message
            ]
                |> String.join "\n\n"

        InvalidLocaleNames invalidLocaleNames ->
            [ title "INVALID LOCALE NAMES" ""
            , [ "I cannot find the locales "
              , invalidLocaleNames
                    |> List.map yellowText
                    |> String.join ", "
              , ". Please check the configuration in elm-translation.json, if these are actually the names of your locales."
              ]
                |> String.concat
                |> String.softWrap 80
            ]
                |> String.join "\n\n"

        MissingTranslation data ->
            let
                ( scope, key ) =
                    data.scopeKey
            in
            [ title "missing translation" data.filePath
            , [ [ "The locale "
                , yellowText data.localeName
                , " does not have a translation for"
                ]
                    |> String.concat
                    |> String.softWrap 80
              , [ printScope scope
                , String.repeat (1 + List.length scope) "  "
                , yellowText (quote key)
                , ": "
                , redText "<missing translation>"
                ]
                    |> String.concat
                    |> indent
              , [ "Either add a translation or configure fallback locales for "
                , data.localeName
                , ". For example the locales "
                , data.availableFallbackLocales
                    |> List.map yellowText
                    |> String.join ", "
                , " have a translation for this key!"
                , "\n"
                ]
                    |> String.concat
                    |> String.softWrap 80
              ]
                |> String.join "\n\n"
            ]
                |> String.join "\n\n"

        IcuErrors data ->
            data.icuErrors
                |> List.map (printIcuError data)
                |> String.join "\n\n"

        JSONSyntax data ->
            [ title "json decode error" data.filePath
            , [ "There was an error while reading the translations for the locale "
              , yellowText data.localeName
              , " from the file "
              , yellowText data.filePath
              , ". The JSON decoder says:"
              ]
                |> String.concat
                |> String.softWrap 80
            , [ data.errorMsg
                    |> indent
              , "\n"
              ]
                |> String.concat
            ]
                |> String.join "\n\n"


printIcuError :
    { rest
        | localeName : String
        , filePath : String
        , scopeKey : ( List String, String )
    }
    -> Translation.Error
    -> String
printIcuError data icuError =
    let
        ( scope, key ) =
            data.scopeKey

        printSource source from to =
            [ printScope scope
            , String.repeat (1 + List.length scope) "  "
            , yellowText (quote key)
            , ": "
            , quote source
            , "\n"
            , String.repeat (6 + String.length key + (2 * List.length scope)) " "
            , [ String.repeat (1 + from) " "
              , String.repeat (1 + to - from) "^"
              ]
                |> String.concat
                |> redText
            ]
                |> String.concat
                |> indent

        printSourceMarkSubMessages source ranges =
            [ printScope scope
            , String.repeat (1 + List.length scope) "  "
            , yellowText (quote key)
            , ": "
            , quote source
            , "\n"
            , String.repeat (8 + String.length key + (2 * List.length scope)) " "
            , ranges
                |> List.sortBy .from
                |> List.foldl
                    (\{ from, to } ( previousTo, arrows ) ->
                        ( to
                        , [ arrows
                          , String.repeat (from - previousTo - 1) " "
                          , String.repeat (1 + to - from) "^"
                          ]
                            |> String.concat
                        )
                    )
                    ( 0, "" )
                |> Tuple.second
                |> redText
            ]
                |> String.concat
                |> indent
    in
    case icuError of
        Translation.ParserError parserError ->
            [ title "icu syntax error" data.filePath
            , [ "A translation for the locale "
              , yellowText data.localeName
              , " contains an ICU syntax error: "
              , [ "I ran into a problem while parsing "
                , parserError.context
                    |> List.head
                    |> Maybe.map .description
                    |> Maybe.map blueText
                    |> Maybe.withDefault ""
                , parserError.context
                    |> List.drop 1
                    |> List.head
                    |> Maybe.map .description
                    |> Maybe.map (\desc -> " of " ++ desc)
                    |> Maybe.withDefault ""
                , ":"
                ]
                    |> String.concat
              ]
                |> String.concat
                |> String.softWrap 80
            , printSource parserError.source
                (parserError.col - 1)
                (parserError.col - 1)
            , [ parserError.problem
                    |> printProblem
                    |> String.concat
              , "\n"
              ]
                |> String.concat
            ]
                |> String.join "\n\n"

        Translation.ArgumentError argumentError ->
            case argumentError.problem of
                Translation.BadArgumentType ->
                    [ title "undeducable argument type" data.filePath
                    , [ "We cannot deduce any argument type from the names "
                      , [ "[ "
                        , (argumentError.placeholder :: argumentError.names)
                            |> List.map quote
                            |> String.join ", "
                        , " ]"
                        ]
                            |> String.concat
                            |> yellowText
                      , " within the following translation for the locale "
                      , yellowText data.localeName
                      , ": "
                      ]
                        |> String.concat
                        |> String.softWrap 80
                    , printSource argumentError.source argumentError.from argumentError.to
                    ]
                        |> String.join "\n\n"

                Translation.ExpectingSingleUnnamedSubMessage ->
                    [ title "expecting single unnamed submessage" data.filePath
                    , [ "We expected only a single unnamed submessage for the placeholder "
                      , yellowText argumentError.placeholder
                      , " in the following translation for the locale "
                      , yellowText data.localeName
                      , ":"
                      ]
                        |> String.concat
                        |> String.softWrap 80
                    , printSourceMarkSubMessages argumentError.source <|
                        List.concat
                            [ argumentError.unnamedSubMessages
                                |> List.sortBy .from
                                |> List.drop 1
                            , argumentError.namedSubMessages
                                |> List.map
                                    (\named ->
                                        { from = named.from
                                        , to = named.to
                                        }
                                    )
                            ]
                    ]
                        |> String.join "\n\n"

                Translation.ExpectingOnlyUnnamedSubMessages ->
                    [ title "expecting only unnamed submessages" data.filePath
                    , [ "We expected unnamed submessages for the placeholder "
                      , yellowText argumentError.placeholder
                      , " in a translation for the locale "
                      , yellowText data.localeName
                      , ":"
                      ]
                        |> String.concat
                        |> String.softWrap 80
                    , printSourceMarkSubMessages
                        argumentError.source
                        argumentError.namedSubMessages
                    ]
                        |> String.join "\n\n"

                Translation.ExpectingOnlyNamedSubMessages expectedNames ->
                    [ title "expecting named submessages" data.filePath
                    , [ "We expected the submessages of the placeholder "
                      , yellowText argumentError.placeholder
                      , " to be one of: "
                      , expectedNames
                            |> List.map quote
                            |> String.join ", "
                      , ".  This was within a translation for the locale "
                      , yellowText data.localeName
                      , ":"
                      ]
                        |> String.concat
                        |> String.softWrap 80
                    , printSourceMarkSubMessages
                        argumentError.source
                        argumentError.unnamedSubMessages
                    ]
                        |> String.join "\n\n"


title : String -> String -> String
title name location =
    [ "\x1B[36m--"
    , name
        |> String.map Char.toUpper
    , "-"
        |> List.repeat (80 - 5 - String.length name - String.length location)
        |> String.concat
    , location
    , "\x1B[0m"
    ]
        |> String.join " "


printScope : List String -> String
printScope scope =
    if List.isEmpty scope then
        "{\n"
    else
        [ "{\n"
        , scope
            |> List.indexedMap
                (\deepness name ->
                    [ String.repeat (deepness + 1) "  "
                    , quote name
                    , ": {"
                    ]
                        |> String.concat
                )
            |> String.join "\n"
        , "\n"
        ]
            |> String.concat


{-| Apply a function which can fail onto each element of
list. If at least one of them fails, return an error with a collection of all
failures.
-}
mapWithErrors : (a -> Result Error b) -> List a -> Result Error (List b)
mapWithErrors func listA =
    listA
        |> List.foldl
            (\a resultListB ->
                case resultListB of
                    Ok listB ->
                        case func a of
                            Ok b ->
                                Ok (b :: listB)

                            Err error ->
                                Err [ error ]

                    Err errors ->
                        case func a of
                            Ok b ->
                                resultListB

                            Err error ->
                                Err (error :: errors)
            )
            (Ok [])
        |> Result.mapError Errors


mapDictWithErrors :
    (comparable -> a -> Result Error b)
    -> Dict comparable a
    -> Result Error (Dict comparable b)
mapDictWithErrors func dictA =
    dictA
        |> Dict.foldl
            (\comparable a resultDictB ->
                case resultDictB of
                    Ok dictB ->
                        case func comparable a of
                            Ok b ->
                                Ok (Dict.insert comparable b dictB)

                            Err error ->
                                Err [ error ]

                    Err errors ->
                        case func comparable a of
                            Ok b ->
                                resultDictB

                            Err error ->
                                Err (error :: errors)
            )
            (Ok Dict.empty)
        |> Result.mapError Errors



---- ICU


flattenProblem :
    Parser.Problem
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
flattenProblem problem =
    flattenProblemHelper problem
        { keywords = []
        , symbols = []
        , others = []
        }
        |> (\collected ->
                { collected
                    | keywords = List.unique collected.keywords
                    , symbols = List.unique collected.symbols
                }
           )


flattenProblemHelper :
    Parser.Problem
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
    ->
        { keywords : List String
        , symbols : List String
        , others : List Parser.Problem
        }
flattenProblemHelper problem collected =
    case problem of
        Parser.ExpectingKeyword keyword ->
            { collected | keywords = keyword :: collected.keywords }

        Parser.ExpectingSymbol symbol ->
            { collected | symbols = symbol :: collected.symbols }

        Parser.BadOneOf problems ->
            problems
                |> List.foldl flattenProblemHelper collected

        _ ->
            { collected | others = problem :: collected.others }


printKeywords : List String -> Maybe (List String)
printKeywords keywords =
    case keywords of
        [] ->
            Nothing

        _ ->
            [ [ "one of the following keywords:\n\n" ]
            , keywords
                |> List.map
                    (\keyword ->
                        [ "    "
                        , greenText keyword
                        ]
                    )
                |> List.intersperse [ ",\n" ]
                |> List.concat
            ]
                |> List.concat
                |> Just


printSymbols : List String -> Maybe (List String)
printSymbols symbols =
    case symbols of
        [] ->
            Nothing

        _ ->
            [ [ "one of the following symbols:\n\n    " ]
            , symbols
                |> List.map
                    (\symbol ->
                        [ "'"
                        , greenText symbol
                        , "'"
                        ]
                    )
                |> List.intersperse [ ",  " ]
                |> List.concat
            ]
                |> List.concat
                |> Just


printProblem : Parser.Problem -> List String
printProblem problem =
    let
        { keywords, symbols, others } =
            flattenProblem problem
    in
    [ [ "I expected " ]
    , [ printKeywords keywords
      , printSymbols symbols
      , case others of
            [] ->
                Nothing

            _ ->
                others
                    |> List.map printSingleProblem
                    |> List.concat
                    |> List.intersperse " or "
                    |> Just
      ]
        |> List.filterMap identity
        |> List.intersperse [ ",\n\nor " ]
        |> List.concat
    , [ "." ]
    ]
        |> List.concat


printSingleProblem : Parser.Problem -> List String
printSingleProblem problem =
    case problem of
        Parser.BadOneOf problems ->
            []

        Parser.BadInt ->
            [ greenText "a number" ]

        Parser.BadRepeat ->
            []

        Parser.ExpectingSymbol symbol ->
            [ "the symbol '"
            , greenText <|
                escapeSymbol symbol
            , "'"
            ]

        Parser.ExpectingKeyword keyword ->
            [ "the keyword '"
            , greenText keyword
            , "'"
            ]

        Parser.ExpectingVariable ->
            [ greenText "a variable" ]

        Parser.Fail message ->
            [ greenText message ]

        _ ->
            [ toString problem ]


escapeSymbol : String -> String
escapeSymbol symbol =
    case symbol of
        "\n" ->
            "\\n"

        "\t" ->
            "\\t"

        _ ->
            symbol



---- HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


quote : String -> String
quote text =
    [ "\""
    , text
    , "\""
    ]
        |> String.concat


redText : String -> String
redText text =
    coloredText red text


greenText : String -> String
greenText text =
    coloredText green text


yellowText : String -> String
yellowText text =
    coloredText yellow text


blueText : String -> String
blueText text =
    coloredText blue text


coloredText : String -> String -> String
coloredText color text =
    [ color
    , text
    , reset
    ]
        |> String.concat


reset : String
reset =
    "\x1B[0m"


red : String
red =
    "\x1B[31m"


green : String
green =
    "\x1B[32m"


blue : String
blue =
    "\x1B[34m"


yellow : String
yellow =
    "\x1B[33m"
