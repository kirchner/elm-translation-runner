module Error
    exposing
        ( Error(..)
        , icuSyntax
        , jsonSyntax
        , mapDictWithErrors
        , mapWithErrors
        , print
        )

import Char
import Dict exposing (Dict)
import Error.Icu
import Parser
import String.Extra as String


type Error
    = Errors (List Error)
    | MissingTranslationsFileFor String
    | MissingTranslationFor String ( List String, String )
    | MissingTranslations
        { locale : String
        , missingKeys : List String
        }
    | IcuSyntaxError
        { file : String
        , key : List String
        , error : Parser.Error
        }
    | JSONSyntaxError
        { fileName : String
        , locale : String
        , errorMsg : String
        }
    | NoTranslationsError
        { fileName : String
        , locale : String
        }


jsonSyntax : String -> String -> String -> Error
jsonSyntax path locale errorMsg =
    JSONSyntaxError
        { fileName = path
        , locale = locale
        , errorMsg = errorMsg
        }


icuSyntax : String -> List String -> Parser.Error -> Error
icuSyntax path key parserError =
    IcuSyntaxError
        { file = path
        , key = key
        , error = parserError
        }


print : Error -> String
print error =
    let
        title name location =
            [ "\x1B[36m--"
            , name
                |> String.map Char.toUpper
            , "-"
                |> List.repeat (80 - 4 - String.length name - String.length location)
                |> String.concat
            , location
            , "\x1B[0m"
            ]
                |> String.join " "
    in
    case error of
        MissingTranslationsFileFor locale ->
            Debug.crash "add error"

        MissingTranslationFor locale key ->
            Debug.crash "add error"

        Errors errors ->
            errors
                |> List.map print
                |> String.join "\n\n"

        IcuSyntaxError { file, key, error } ->
            [ title "syntax error" file
            , error
                |> Error.Icu.print
            ]
                |> String.join "\n\n"

        MissingTranslations { locale, missingKeys } ->
            [ title "missing translations" ""
            , [ [ "The locale '"
                , locale
                , "' is missing translations, which are given in another locale. The translation keys, which we cannot find translations for, are:"
                ]
                    |> String.concat
                    |> String.softWrap 80
              , [ missingKeys
                    |> String.join ", "
                    |> indent
                , "\n"
                ]
                    |> String.concat
              ]
                |> String.join "\n\n"
            ]
                |> String.join "\n\n"

        JSONSyntaxError { fileName, locale, errorMsg } ->
            [ title "json decode error" fileName
            , [ "There was an error while reading the translations for the locale '"
              , locale
              , "' in the file '"
              , fileName
              , ". The JSON decoder says:"
              ]
                |> String.concat
                |> String.softWrap 80
            , "\n\n"
            , errorMsg
                |> String.softWrap 70
                |> indent
            , "\n"
            ]
                |> String.concat

        NoTranslationsError { fileName, locale } ->
            [ title "no translations error" fileName
            , [ "There was an error while reading the translations for the locale '"
              , locale
              , "' in the file '"
              , fileName
              , ". The file does not contain any translations."
              ]
                |> String.concat
                |> String.softWrap 80
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



---- HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"
