module Main exposing (main)

import Char
import Dict exposing (Dict)
import Error
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Parser
import Platform exposing (programWithFlags)
import Ports
import Set exposing (Set)
import String.Extra as String
import Translation exposing (ArgType(..))


main : Program Value Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }



---- CONFIG


type alias Config =
    { sourceDirectory : String
    , modulePrefix : List String
    , moduleName : String
    , locales : List LocaleConfig
    }


type alias LocaleConfig =
    { name : String
    , code : String
    , files : List FileConfig
    , fallbacks : List String
    }


type alias FileConfig =
    { path : String
    , modulePrefix : List String
    }


requiredFilesPaths : Config -> List String
requiredFilesPaths config =
    config.locales
        |> List.foldl
            (\locale files ->
                locale.files
                    |> List.map .path
                    |> List.append files
            )
            []


configDecoder : Decoder Config
configDecoder =
    Decode.succeed Config
        |> Decode.required "source-directory" Decode.string
        |> Decode.required "module-prefix" modulePrefixDecoder
        |> Decode.required "module-name" Decode.string
        |> Decode.required "locales" (Decode.list localeConfigDecoder)


localeConfigDecoder : Decoder LocaleConfig
localeConfigDecoder =
    Decode.succeed LocaleConfig
        |> Decode.required "name" Decode.string
        |> Decode.required "code" Decode.string
        |> Decode.required "files" (Decode.list fileConfigDecoder)
        |> Decode.required "fallbacks" (Decode.list Decode.string)


fileConfigDecoder : Decoder FileConfig
fileConfigDecoder =
    Decode.succeed FileConfig
        |> Decode.required "path" Decode.string
        |> Decode.required "module-prefix" modulePrefixDecoder


modulePrefixDecoder : Decoder (List String)
modulePrefixDecoder =
    Decode.string
        |> Decode.map
            (\string ->
                if string == "" then
                    []
                else
                    String.split "." string
            )



---- MODEL


type alias Model =
    { config : Config
    , files : Dict String String
    }



---- INIT


init : Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue configDecoder value of
        Ok config ->
            ( { config = config
              , files = Dict.empty
              }
            , config.locales
                |> List.map (.files >> List.map (.path >> Ports.fetchFile))
                |> List.concat
                |> Cmd.batch
            )

        Err errorMsg ->
            Debug.crash errorMsg



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Ports.fileReceived FileReceived


type alias ReceivedFile =
    { path : String
    , content : String
    }


fileDecoder : Decoder ReceivedFile
fileDecoder =
    Decode.succeed ReceivedFile
        |> Decode.required "path" Decode.string
        |> Decode.required "content" Decode.string



---- UPDATE


type Msg
    = FileReceived Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FileReceived value ->
            case Decode.decodeValue fileDecoder value of
                Ok file ->
                    { model
                        | files =
                            model.files
                                |> Dict.insert file.path file.content
                    }
                        |> processFiles

                Err errorMsg ->
                    Debug.crash errorMsg


processFiles : Model -> ( Model, Cmd Msg )
processFiles model =
    let
        requiredFiles =
            model.config
                |> requiredFilesPaths
                |> List.sort

        receivedFiles =
            model.files
                |> Dict.keys
                |> List.sort
    in
    if requiredFiles /= receivedFiles then
        ( model, Cmd.none )
    else
        let
            locales =
                model.config.locales
                    |> List.map
                        (\locale ->
                            locale.files
                                |> List.map
                                    (\file ->
                                        { locale = locale.code
                                        , fileName = file.path
                                        , rawJson =
                                            Dict.get file.path model.files
                                                |> Maybe.withDefault "{}"
                                        }
                                    )
                        )
                    |> List.concat

            collect byScope =
                { allLocales =
                    collectLocales byScope
                        |> List.map
                            (\localeName ->
                                ( localeName
                                , model.config.locales
                                    |> List.filter (\locale -> locale.name == localeName)
                                    |> List.head
                                    |> Maybe.map (\locale -> locale.fallbacks)
                                    |> Maybe.withDefault []
                                )
                            )
                , translationsList =
                    collectTranslationsList
                        model.config.modulePrefix
                        model.config.moduleName
                        byScope
                }
        in
        ( model
        , collectTranslations locales
            |> Result.andThen
                (collect
                    >> translationsFiles model.config.modulePrefix
                    >> Result.map
                        (\( files, { allLocales, translationsList } ) ->
                            [ translationsList
                                |> translationsRouterFiles model.config.modulePrefix
                                    (allLocales
                                        |> List.map Tuple.first
                                        |> Set.fromList
                                    )
                                |> List.map writeFile
                                |> Cmd.batch
                            , files
                                |> List.map writeFile
                                |> Cmd.batch
                            , allLocales
                                |> List.map Tuple.first
                                |> Set.fromList
                                |> localesFile model.config.modulePrefix
                                |> writeFile
                            ]
                                |> Cmd.batch
                        )
                )
            |> (\result ->
                    case result of
                        Ok cmd ->
                            cmd

                        Err error ->
                            error
                                |> reportError
               )
        )


collectTranslationsList :
    List String
    -> String
    -> Dict Scope (Dict Key (Dict Locale String))
    -> List ( Module, List ( String, Dict Locale String ) )
collectTranslationsList modules root byScope =
    let
        allLocales =
            collectLocales byScope
    in
    byScope
        |> Dict.toList
        |> List.map
            (\( scope, byKey ) ->
                let
                    scopeLength =
                        List.length scope
                in
                case
                    ( List.take (scopeLength - 1) scope
                    , List.drop (scopeLength - 1) scope
                    )
                of
                    ( modules, name :: [] ) ->
                        ( Module (modules ++ [ root ]) name
                        , Dict.toList byKey
                        )

                    ( modules, [] ) ->
                        ( Module modules root
                        , Dict.toList byKey
                        )

                    _ ->
                        Debug.crash "should not happen"
            )



---- FILES


type alias File =
    { directory : List String
    , name : String
    , content : String
    }


writeFile : File -> Cmd msg
writeFile { directory, name, content } =
    [ ( "scope"
      , (directory ++ [ name ])
            |> List.map Encode.string
            |> Encode.list
      )
    , ( "content"
      , [ content
            |> String.split "\n"
            |> List.map
                (\line ->
                    if
                        line
                            |> String.toList
                            |> List.all (\char -> char == ' ')
                    then
                        ""
                    else
                        line
                )
            |> String.join "\n"
        , "\n"
        ]
            |> String.concat
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.writeModule



---- ERRORS


type Error
    = Errors (List Error)
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


reportError : Error -> Cmd msg
reportError error =
    [ ( "error"
      , error
            |> printError
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.reportError


printError : Error -> String
printError error =
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
        Errors errors ->
            errors
                |> List.map printError
                |> String.join "\n\n"

        IcuSyntaxError { file, key, error } ->
            [ title "syntax error" file
            , error
                |> Error.print
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



---- TRANSLATIONS


type alias Scope =
    List String


type alias Locale =
    String


type alias Key =
    String


type Directory
    = Directory (List ( Key, Directory ))
    | Entry String


directoryDecoder : Decoder Directory
directoryDecoder =
    Decode.oneOf
        [ Decode.keyValuePairs (Decode.lazy (\_ -> directoryDecoder))
            |> Decode.map Directory
        , Decode.string
            |> Decode.map Entry
        ]



---- COLLECT TRANSLATIONS


collectTranslations :
    List
        { locale : String
        , fileName : String
        , rawJson : String
        }
    -> Result Error (Dict Scope (Dict Key (Dict Locale String)))
collectTranslations rawData =
    rawData
        |> doCollectingErrors
            (\{ locale, fileName, rawJson } ->
                case Decode.decodeString directoryDecoder rawJson of
                    Ok (Directory entries) ->
                        Ok
                            ( locale
                            , translationsByScope entries
                            )

                    Ok _ ->
                        Err <|
                            NoTranslationsError
                                { fileName = fileName
                                , locale = locale
                                }

                    Err error ->
                        Err <|
                            JSONSyntaxError
                                { fileName = fileName
                                , locale = locale
                                , errorMsg = error
                                }
            )
        |> Result.map collectTranslationsHelp


collectTranslationsHelp :
    List ( Locale, Dict Scope (Dict Key String) )
    -> Dict Scope (Dict Key (Dict Locale String))
collectTranslationsHelp translations =
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


translationsByScope : List ( Key, Directory ) -> Dict Scope (Dict Key String)
translationsByScope entries =
    entries
        |> List.map (uncurry (translationsByScopeHelp []))
        |> List.foldr merge Dict.empty


translationsByScopeHelp :
    List String
    -> Key
    -> Directory
    -> Dict (List String) (Dict String String)
translationsByScopeHelp scope name directory =
    case directory of
        Entry newMessage ->
            Dict.singleton (List.reverse scope) (Dict.singleton name newMessage)

        Directory entries ->
            entries
                |> List.map (uncurry (translationsByScopeHelp (name :: scope)))
                |> List.foldr merge Dict.empty



---- COLLECT LOCALES


collectLocales : Dict Scope (Dict Key (Dict Locale String)) -> List String
collectLocales translations =
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
        |> Set.toList



---- GENERATE LOCALES MODULE FILE


localesFile : List String -> Set String -> File
localesFile modules locales =
    { directory = modules
    , name = "Locales"
    , content =
        [ generateModuleDeclaration modules "Locales"
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
    }



---- GENERATE TRANSLATIONS ROUTER MODULE FILES


type Module
    = Module (List String) String


translationsRouterFiles :
    List String
    -> Set String
    -> List ( Module, List ( Key, Dict Locale String ) )
    -> List File
translationsRouterFiles modules locales translations =
    List.map
        (uncurry (translationsRouterFile modules locales))
        translations


translationsRouterFile :
    List String
    -> Set String
    -> Module
    -> List ( Key, Dict Locale String )
    -> File
translationsRouterFile modules locales (Module scope name) translations =
    let
        actualName =
            String.toSentenceCase name

        actualModules =
            (modules ++ scope)
                |> List.map String.toSentenceCase
    in
    { directory = actualModules
    , name = actualName
    , content =
        [ [ generateModuleDeclaration actualModules (String.toSentenceCase actualName)
          , [ [ generateImport modules "Locales"
              , generateImportExposing [ "Translation" ] [] "Translation"
              ]
            , locales
                |> Set.toList
                |> List.map String.toSentenceCase
                |> List.map (generateQualifiedImport (actualModules ++ [ actualName ]))
            ]
                |> List.concat
                |> joinLines
          ]
            |> joinLinesWith 1
        , translations
            |> List.map (uncurry (generateTranslationFunction locales))
            |> joinLinesWith 2
        ]
            |> joinLinesWith 2
    }


generateTranslationFunction : Set String -> Key -> Dict Locale String -> String
generateTranslationFunction locales key byLocale =
    [ [ key
      , ": Locale ->"
      , byLocale
            |> Dict.values
            |> List.head
            |> Maybe.andThen
                (Translation.toElmType cldrToArgType
                    >> Result.toMaybe
                )
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
                        |> joinLines
                )
            |> joinLinesWith 1
            |> indent
      ]
        |> joinLines
        |> indent
    ]
        |> joinLines



---- GENERATE TRANSLATIONS MODULE FILES


translationsFiles :
    List String
    ->
        { allLocales : List ( Locale, List Locale )
        , translationsList : List ( Module, List ( Key, Dict Locale String ) )
        }
    ->
        Result Error
            ( List File
            , { allLocales : List ( Locale, List Locale )
              , translationsList : List ( Module, List ( Key, Dict Locale String ) )
              }
            )
translationsFiles modules ({ allLocales, translationsList } as data) =
    allLocales
        |> doCollectingErrors (uncurry (translationsFilesForLocale modules translationsList))
        |> Result.map (\list -> ( List.concat list, data ))


translationsFilesForLocale :
    List String
    -> List ( Module, List ( Key, Dict Locale String ) )
    -> Locale
    -> List Locale
    -> Result Error (List File)
translationsFilesForLocale modules translations locale fallbacks =
    translations
        |> doCollectingErrors (uncurry (translationsFileForLocale modules locale fallbacks))


translationsFileForLocale :
    List String
    -> Locale
    -> List Locale
    -> Module
    -> List ( Key, Dict Locale String )
    -> Result Error File
translationsFileForLocale modules locale fallbacks (Module scope name) translations =
    let
        actualModules =
            (modules ++ scope ++ [ name ])
                |> List.map String.toSentenceCase

        actualModule =
            String.toSentenceCase locale
    in
    translations
        |> fetchTranslationsForLocale locale fallbacks
        |> Result.andThen
            (doCollectingErrors
                (\( name, fetchedTranslation ) ->
                    generateMessage name fetchedTranslation
                        |> Result.mapError
                            (\error ->
                                IcuSyntaxError
                                    { file = ""
                                    , key = [ name ]
                                    , error = error
                                    }
                            )
                )
                >> Result.map
                    (\messages ->
                        { directory = actualModules
                        , name = actualModule
                        , content =
                            [ [ generateModuleDeclaration actualModules
                                    (String.toSentenceCase locale)
                              , [ generateImport [] "Translation"
                                , generateImportExposing [ "Node" ] [] "VirtualDom"
                                , generateImport [ "Translation" ] actualModule
                                ]
                                    |> joinLines
                              ]
                                |> joinLinesWith 1
                            , joinLinesWith 2 messages
                            ]
                                |> joinLinesWith 2
                        }
                    )
            )


type FetchedTranslation
    = Final String
    | Fallback String


fetchTranslationsForLocale :
    Locale
    -> List Locale
    -> List ( Key, Dict Locale String )
    -> Result Error (List ( Key, FetchedTranslation ))
fetchTranslationsForLocale locale fallbacks translations =
    let
        getTranslation byLocale =
            case Dict.get locale byLocale of
                Just translation ->
                    Just (Final translation)

                Nothing ->
                    fallbacks
                        |> List.foldl
                            (\fallbackLocale maybeTranslation ->
                                case maybeTranslation of
                                    Just _ ->
                                        maybeTranslation

                                    Nothing ->
                                        Dict.get fallbackLocale byLocale
                                            |> Maybe.map Fallback
                            )
                            Nothing
    in
    translations
        |> List.foldl
            (\( key, byLocale ) translationsResult ->
                case translationsResult of
                    Ok translations ->
                        case getTranslation byLocale of
                            Just translation ->
                                Ok (( key, translation ) :: translations)

                            Nothing ->
                                Err [ key ]

                    Err missingKeys ->
                        case getTranslation byLocale of
                            Just _ ->
                                translationsResult

                            Nothing ->
                                Err (key :: missingKeys)
            )
            (Ok [])
        |> Result.mapError
            (\missingKeys ->
                MissingTranslations
                    { locale = locale
                    , missingKeys = missingKeys
                    }
            )


generateMessage : String -> FetchedTranslation -> Result Parser.Error String
generateMessage name fetchedTranslation =
    case fetchedTranslation of
        Final translation ->
            Translation.toElm cldrToArgType (String.camelize name) translation

        Fallback translation ->
            Translation.toFallbackElm cldrToArgType (String.camelize name) translation


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



---- CODE GENERATION HELPER


indent : String -> String
indent text =
    text
        |> String.split "\n"
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"


joinLines : List String -> String
joinLines =
    joinLinesWith 0


joinLinesWith : Int -> List String -> String
joinLinesWith lineBreakCount lines =
    let
        lineBreaks =
            "\n"
                |> List.repeat (lineBreakCount + 1)
                |> String.concat
    in
    lines
        |> String.join lineBreaks


generateModuleDeclaration : List String -> String -> String
generateModuleDeclaration modules name =
    [ "module"
    , modules
        ++ [ name ]
        |> String.join "."
    , "exposing (..)"
    ]
        |> String.join " "


generateImport : List String -> String -> String
generateImport modules name =
    [ "import"
    , modules
        ++ [ name ]
        |> String.join "."
    , "exposing (..)"
    ]
        |> String.join " "


generateImportExposing : List String -> List String -> String -> String
generateImportExposing exposed modules name =
    [ "import"
    , modules
        ++ [ name ]
        |> String.join "."
    , "exposing"
    , [ "("
      , exposed
            |> String.join ", "
      , ")"
      ]
        |> String.concat
    ]
        |> String.join " "


generateQualifiedImport : List String -> String -> String
generateQualifiedImport modules name =
    [ "import"
    , modules
        ++ [ name ]
        |> String.join "."
    , "as"
    , String.toSentenceCase name
    , "exposing (..)"
    ]
        |> String.join " "



---- ERROR HELPER


doCollectingErrors : (a -> Result Error b) -> List a -> Result Error (List b)
doCollectingErrors func listA =
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
