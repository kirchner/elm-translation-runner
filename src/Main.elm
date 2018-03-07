module Main exposing (main)

import Char
import Dict exposing (Dict)
import Error exposing (Error(..), mapDictWithErrors, mapWithErrors)
import Generate exposing (indent, joinLines, joinLinesWith)
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



---- MODEL


type alias Model =
    { config : Config
    , files : Dict String String
    }



---- FILE


type alias File =
    { directory : List String
    , name : String
    , content : String
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
    , filePath : String
    , fallbacks : List String
    }


requiredFilesPaths : Config -> List String
requiredFilesPaths config =
    config.locales
        |> List.map .filePath


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
        |> Decode.required "file-path" Decode.string
        |> Decode.required "fallbacks" (Decode.list Decode.string)


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



---- INIT


init : Value -> ( Model, Cmd Msg )
init value =
    case Decode.decodeValue configDecoder value of
        Ok config ->
            ( { config = config
              , files = Dict.empty
              }
            , config.locales
                |> List.map (.filePath >> Ports.fetchFile)
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
        ( model
        , case
            model.config.locales
                |> Error.mapWithErrors
                    (\localeConfig ->
                        Dict.get localeConfig.filePath model.files
                            |> Maybe.withDefault "{}"
                            |> processFile localeConfig.name localeConfig.filePath
                            |> Result.map (\translations -> ( localeConfig.name, translations ))
                    )
                |> Result.map Dict.fromList
                |> Result.andThen
                    (\translations ->
                        model.config.locales
                            |> Error.mapWithErrors
                                (\localeConfig ->
                                    collectTranslationCodeForLocale
                                        localeConfig
                                        translations
                                        |> Result.map
                                            (generateTranslationFiles
                                                model.config.modulePrefix
                                                model.config.moduleName
                                                localeConfig
                                            )
                                )
                            |> Result.map List.concat
                            |> Result.map
                                (List.append <|
                                    generateLocalesFile
                                        model.config.modulePrefix
                                        model.config.locales
                                        :: generateTranslationRouterFiles
                                            model.config.modulePrefix
                                            model.config.moduleName
                                            model.config.locales
                                            translations
                                )
                    )
          of
            Err error ->
                reportError error

            Ok files ->
                files
                    |> List.map writeFile
                    |> Cmd.batch
        )


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


reportError : Error -> Cmd msg
reportError error =
    [ ( "error"
      , error
            |> Error.print
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.reportError



---- COLLECT TRANSLATIONS


type alias TranslationCode =
    { final : String
    , fallback : String
    , typeSignature : String
    }


processFile :
    String
    -> String
    -> String
    -> Result Error (Dict ( List String, String ) TranslationCode)
processFile locale path content =
    let
        flattenDirectory scope directory =
            case directory of
                Entry entry ->
                    case List.reverse scope of
                        [] ->
                            Dict.empty

                        name :: reversedScope ->
                            Dict.singleton ( List.reverse reversedScope, name ) entry

                Directory namedSubDirectories ->
                    namedSubDirectories
                        |> List.map
                            (\( name, subDirectory ) ->
                                flattenDirectory (scope ++ [ name ]) subDirectory
                            )
                        |> List.foldl Dict.union Dict.empty

        generateTranslationsCode path translations =
            translations
                |> mapDictWithErrors
                    (\( scope, name ) content ->
                        Translation.toElm cldrToArgType name content
                            |> Result.mapError
                                (Error.icuSyntax path (scope ++ [ name ]))
                            |> Result.andThen
                                (\final ->
                                    Translation.toFallbackElm cldrToArgType name content
                                        |> Result.mapError
                                            (Error.icuSyntax path (scope ++ [ name ]))
                                        |> Result.andThen
                                            (\fallback ->
                                                Translation.toElmType cldrToArgType content
                                                    |> Result.mapError
                                                        (Error.icuSyntax path (scope ++ [ name ]))
                                                    |> Result.map (TranslationCode final fallback)
                                            )
                                )
                    )
    in
    content
        |> Decode.decodeString directoryDecoder
        |> Result.mapError (Error.jsonSyntax path locale)
        |> Result.map (flattenDirectory [])
        |> Result.andThen (generateTranslationsCode path)


collectTranslationCodeForLocale :
    LocaleConfig
    -> Dict String (Dict ( List String, String ) TranslationCode)
    -> Result Error (Dict ( List String, String ) String)
collectTranslationCodeForLocale localeConfig translations =
    let
        keys =
            translations
                |> Dict.values
                |> List.map (Dict.keys >> Set.fromList)
                |> List.foldl Set.union Set.empty
                |> Set.toList

        getFallbackTranslationCode key =
            localeConfig.fallbacks
                |> List.foldl
                    (\fallbackLocale maybeTranslationCode ->
                        case maybeTranslationCode of
                            Just _ ->
                                maybeTranslationCode

                            Nothing ->
                                Dict.get fallbackLocale translations
                                    |> Maybe.andThen
                                        (\translationsForFallbackLocale ->
                                            Dict.get key translationsForFallbackLocale
                                                |> Maybe.map .fallback
                                        )
                    )
                    Nothing
    in
    Dict.get localeConfig.name translations
        |> Result.fromMaybe (MissingTranslationsFileFor localeConfig.name)
        |> Result.andThen
            (\translationsForLocale ->
                keys
                    |> Error.mapWithErrors
                        (\key ->
                            Dict.get key translationsForLocale
                                |> Maybe.map (.final >> Just)
                                |> Maybe.withDefault (getFallbackTranslationCode key)
                                |> Result.fromMaybe
                                    (MissingTranslationFor localeConfig.name key)
                                |> Result.map (\code -> ( key, code ))
                        )
                    |> Result.map Dict.fromList
            )


generateTranslationFiles :
    List String
    -> String
    -> LocaleConfig
    -> Dict ( List String, String ) String
    -> List File
generateTranslationFiles modulePrefix moduleName localeConfig translationCodes =
    translationCodes
        |> Dict.foldl
            (\( scope, key ) code byModule ->
                byModule
                    |> Dict.update scope
                        (\maybeCodes ->
                            case maybeCodes of
                                Nothing ->
                                    Just [ ( key, code ) ]

                                Just codes ->
                                    Just (( key, code ) :: codes)
                        )
            )
            Dict.empty
        |> Dict.toList
        |> List.map
            (\( scope, translations ) ->
                let
                    sanitizedScope =
                        List.map String.toSentenceCase scope

                    sanitizedName =
                        String.toSentenceCase localeConfig.name

                    sanitizedCode =
                        String.toSentenceCase localeConfig.code
                in
                { directory = modulePrefix ++ (moduleName :: sanitizedScope)
                , name = sanitizedName
                , content =
                    [ [ Generate.moduleDeclaration
                            (modulePrefix ++ (moduleName :: sanitizedScope))
                            sanitizedName
                      , [ Generate.import_ [] "Translation"
                        , Generate.import_ [ "Translation" ] sanitizedCode
                        ]
                            |> joinLines
                      ]
                        |> joinLinesWith 1
                    , translations
                        |> List.sortBy Tuple.first
                        |> List.map Tuple.second
                        |> joinLinesWith 2
                    ]
                        |> joinLinesWith 2
                }
            )


generateTranslationRouterFiles :
    List String
    -> String
    -> List LocaleConfig
    -> Dict String (Dict ( List String, String ) TranslationCode)
    -> List File
generateTranslationRouterFiles modulePrefix moduleName locales translations =
    translations
        |> Dict.values
        |> List.foldl
            (\translations byModule ->
                translations
                    |> Dict.foldl
                        (\( scope, key ) code byModule ->
                            byModule
                                |> Dict.update scope
                                    (\maybeKeysCodes ->
                                        case maybeKeysCodes of
                                            Nothing ->
                                                Just [ ( key, code.typeSignature ) ]

                                            Just keysCodes ->
                                                Just (( key, code.typeSignature ) :: keysCodes)
                                    )
                                |> Dict.map (\_ -> Dict.fromList >> Dict.toList)
                        )
                        byModule
            )
            Dict.empty
        |> Dict.toList
        |> List.map
            (\( scope, keysCodes ) ->
                let
                    ( directory, name ) =
                        case List.reverse scope of
                            [] ->
                                ( modulePrefix
                                , moduleName
                                )

                            rawName :: rawScope ->
                                ( [ modulePrefix
                                  , [ moduleName ]
                                  , rawScope
                                        |> List.reverse
                                        |> List.map String.toSentenceCase
                                  ]
                                    |> List.concat
                                , String.toSentenceCase rawName
                                )
                in
                { directory = directory
                , name = name
                , content =
                    [ [ Generate.moduleDeclaration directory name
                      , [ [ Generate.import_ modulePrefix "Locales"
                          , Generate.importExposing [ "Translation" ] [] "Translation"
                          ]
                        , locales
                            |> List.map (.name >> String.toSentenceCase)
                            |> List.map (Generate.qualifiedImport (directory ++ [ name ]))
                        ]
                            |> List.concat
                            |> joinLines
                      ]
                        |> joinLinesWith 1
                    , keysCodes
                        |> List.map
                            (\( key, code ) ->
                                [ String.join " " [ key, ": Locale ->", code ]
                                , String.join " " [ key, "locale =" ]
                                , [ "case locale of"
                                  , locales
                                        |> List.map
                                            (\locale ->
                                                let
                                                    sanitizedLocaleName =
                                                        String.toSentenceCase locale.name
                                                in
                                                [ sanitizedLocaleName ++ " ->"
                                                , [ sanitizedLocaleName, ".", key ]
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
                            )
                        |> joinLinesWith 2
                    ]
                        |> joinLinesWith 2
                }
            )


generateLocalesFile : List String -> List LocaleConfig -> File
generateLocalesFile modulePrefix locales =
    { directory = modulePrefix
    , name = "Locales"
    , content =
        [ Generate.moduleDeclaration modulePrefix "Locales"
        , [ "type Locale"
          , [ "= "
            , locales
                |> List.map (.name >> String.toSentenceCase)
                |> String.join "\n| "
            ]
                |> String.concat
                |> indent
          ]
            |> String.join "\n"
        ]
            |> String.join "\n\n\n"
    }



---- DECODER


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
