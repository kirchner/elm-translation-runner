module Main exposing (main)

import Data.Pluralization as Pluralization exposing (Pluralization)
import Data.Printer as Printer exposing (Printer)
import Dict exposing (Dict)
import Error exposing (Error, mapDictWithErrors, mapWithErrors)
import Generate
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Platform exposing (programWithFlags)
import Ports
import Set exposing (Set)
import String.Extra as String


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
    , onlyFor : Maybe String
    , for : Maybe (List String)
    , cldrHelper : Dict String CldrHelper
    }


type alias LocaleConfig =
    { name : String
    , code : String
    , filePath : String
    , fallbacks : List String
    }


type alias CldrHelper =
    { printers : Dict String Printer
    , pluralizations : Dict String Pluralization
    }


emptyCldrHelper : CldrHelper
emptyCldrHelper =
    { printers = Dict.empty
    , pluralizations = Dict.empty
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
        |> Decode.required "only-for" (Decode.nullable Decode.string)
        |> Decode.required "for" (Decode.nullable (Decode.list Decode.string))
        |> Decode.required "cldr" (Decode.dict cldrHelperDecoder)


cldrHelperDecoder : Decoder CldrHelper
cldrHelperDecoder =
    Decode.succeed CldrHelper
        |> Decode.required "printers" (Decode.dict Printer.decoder)
        |> Decode.required "pluralizations" (Decode.dict Pluralization.decoder)


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
                        let
                            content =
                                Dict.get localeConfig.filePath model.files
                                    |> Maybe.withDefault "{}"
                        in
                        Dict.get localeConfig.name model.config.cldrHelper
                            |> Maybe.withDefault emptyCldrHelper
                            |> (\{ printers } ->
                                    processFile printers
                                        localeConfig.name
                                        localeConfig.filePath
                                        content
                                        |> Result.map (\translations -> ( localeConfig.name, translations ))
                               )
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
                            |> Result.andThen
                                (\translationFiles ->
                                    case ( model.config.for, model.config.onlyFor ) of
                                        ( Just for, Nothing ) ->
                                            case
                                                for
                                                    |> List.foldl
                                                        (\localeName maybeInvalidLocaleNames ->
                                                            case maybeInvalidLocaleNames of
                                                                Nothing ->
                                                                    if
                                                                        List.member localeName
                                                                            (List.map .name model.config.locales)
                                                                    then
                                                                        Nothing
                                                                    else
                                                                        Just [ localeName ]

                                                                Just invalidLocaleNames ->
                                                                    if
                                                                        List.member localeName
                                                                            (List.map .name model.config.locales)
                                                                    then
                                                                        maybeInvalidLocaleNames
                                                                    else
                                                                        Just (localeName :: invalidLocaleNames)
                                                        )
                                                        Nothing
                                            of
                                                Nothing ->
                                                    let
                                                        usedLocales =
                                                            model.config.locales
                                                                |> List.filterMap
                                                                    (\locale ->
                                                                        if
                                                                            List.member locale.name
                                                                                for
                                                                        then
                                                                            Just locale
                                                                        else
                                                                            Nothing
                                                                    )
                                                    in
                                                    [ generateLocalesFile
                                                        model.config.modulePrefix
                                                        usedLocales
                                                        :: generateTranslationRouterFiles
                                                            model.config.modulePrefix
                                                            model.config.moduleName
                                                            usedLocales
                                                            translations
                                                    , translationFiles
                                                    ]
                                                        |> List.concat
                                                        |> Ok

                                                Just invalidLocaleNames ->
                                                    Err (Error.InvalidLocaleNames invalidLocaleNames)

                                        ( Nothing, Just onlyFor ) ->
                                            if List.member onlyFor (List.map .name model.config.locales) then
                                                [ generateTrivialTranslationRouterFiles
                                                    model.config.modulePrefix
                                                    model.config.moduleName
                                                    onlyFor
                                                    translations
                                                , translationFiles
                                                ]
                                                    |> List.concat
                                                    |> Ok
                                            else
                                                Err (Error.InvalidLocaleNames [ onlyFor ])

                                        ( Nothing, Nothing ) ->
                                            [ generateLocalesFile
                                                model.config.modulePrefix
                                                model.config.locales
                                                :: generateTranslationRouterFiles
                                                    model.config.modulePrefix
                                                    model.config.moduleName
                                                    model.config.locales
                                                    translations
                                            , translationFiles
                                            ]
                                                |> List.concat
                                                |> Ok

                                        ( Just _, Just _ ) ->
                                            Debug.crash "TODO: report error"
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
      , [ Error.print error
        , Error.epilog
        ]
            |> String.join "\n\n"
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
    Dict String Printer
    -> String
    -> String
    -> String
    -> Result Error (Dict ( List String, String ) TranslationCode)
processFile printers locale path content =
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
                        Generate.function toArgument scope name content
                            |> Result.mapError
                                (\icuErrors ->
                                    Error.IcuErrors
                                        { localeName = locale
                                        , filePath = path
                                        , scopeKey = ( scope, name )
                                        , icuErrors = icuErrors
                                        }
                                )
                            |> Result.andThen
                                (\final ->
                                    Generate.function toArgument scope name content
                                        |> Result.mapError
                                            (\icuErrors ->
                                                Error.IcuErrors
                                                    { localeName = locale
                                                    , filePath = path
                                                    , scopeKey = ( scope, name )
                                                    , icuErrors = icuErrors
                                                    }
                                            )
                                        |> Result.andThen
                                            (\fallback ->
                                                Generate.typeSignature toArgument content
                                                    |> Result.mapError
                                                        (\icuErrors ->
                                                            Error.IcuErrors
                                                                { localeName = locale
                                                                , filePath = path
                                                                , scopeKey = ( scope, name )
                                                                , icuErrors = icuErrors
                                                                }
                                                        )
                                                    |> Result.map
                                                        (TranslationCode
                                                            (Tuple.first final)
                                                            (Tuple.first fallback)
                                                        )
                                            )
                                )
                    )

        camelizeNames names =
            names
                |> String.join "-"
                |> String.camelize

        toArgument names =
            case names of
                [] ->
                    Just Generate.String

                "node" :: [] ->
                    Just Generate.Node

                "plural" :: otherNames ->
                    printers
                        |> Dict.get (camelizeNames otherNames)
                        |> Maybe.andThen
                            (\printer ->
                                if printer.type_ == Printer.Float then
                                    Just <|
                                        Generate.Cardinal
                                            { name = camelizeNames otherNames
                                            , moduleName = printer.module_
                                            }
                                else
                                    Nothing
                            )

                "selectordinal" :: otherNames ->
                    printers
                        |> Dict.get (camelizeNames otherNames)
                        |> Maybe.andThen
                            (\printer ->
                                if printer.type_ == Printer.Float then
                                    Just <|
                                        Generate.Ordinal
                                            { name = camelizeNames otherNames
                                            , moduleName = printer.module_
                                            }
                                else
                                    Nothing
                            )

                _ ->
                    printers
                        |> Dict.get (camelizeNames names)
                        |> Maybe.map
                            (\printer ->
                                let
                                    function =
                                        { name = camelizeNames names
                                        , moduleName = printer.module_
                                        }
                                in
                                case printer.type_ of
                                    Printer.Delimited ->
                                        Generate.Delimited function

                                    Printer.StaticList ->
                                        Generate.StaticList function

                                    Printer.List ->
                                        Generate.List function

                                    Printer.Float ->
                                        Generate.Float function

                                    Printer.Date ->
                                        Generate.Date function

                                    Printer.Time ->
                                        Generate.Time function
                            )
    in
    content
        |> Decode.decodeString directoryDecoder
        |> Result.mapError
            (\errorMsg ->
                Error.JSONSyntax
                    { localeName = locale
                    , filePath = path
                    , errorMsg = errorMsg
                    }
            )
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
        |> Result.fromMaybe
            (Error.Unexpected <|
                String.concat
                    [ "We tried to get the translations for the locale '"
                    , localeConfig.name
                    , "' but they were not there. Actually, we thought this would never happen! Please open an issue on github.com/kirchner/elm-translation-runner including this error message so we can fix it."
                    ]
            )
        |> Result.andThen
            (\translationsForLocale ->
                keys
                    |> Error.mapWithErrors
                        (\key ->
                            let
                                availableFallbackLocales =
                                    translations
                                        |> Dict.toList
                                        |> List.filterMap
                                            (\( locale, translations ) ->
                                                case Dict.get key translations of
                                                    Nothing ->
                                                        Nothing

                                                    Just _ ->
                                                        Just locale
                                            )
                            in
                            Dict.get key translationsForLocale
                                |> Maybe.map (.final >> Just)
                                |> Maybe.withDefault (getFallbackTranslationCode key)
                                |> Result.fromMaybe
                                    (Error.MissingTranslation
                                        { localeName = localeConfig.name
                                        , filePath = localeConfig.filePath
                                        , availableFallbackLocales = availableFallbackLocales
                                        , scopeKey = key
                                        }
                                    )
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
                    [ [ moduleDeclaration
                            (modulePrefix ++ (moduleName :: sanitizedScope))
                            sanitizedName
                      , [ import_ [] ("Cldr." ++ sanitizedCode)
                        , import_ [] "Text"
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
                    [ [ moduleDeclaration directory name
                      , [ [ import_ modulePrefix "Locales"
                          , importExposing [ "Static", "Text" ] [] "Text"
                          ]
                        , locales
                            |> List.map (.name >> String.toSentenceCase)
                            |> List.map (qualifiedImport (directory ++ [ name ]))
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


generateTrivialTranslationRouterFiles :
    List String
    -> String
    -> String
    -> Dict String (Dict ( List String, String ) TranslationCode)
    -> List File
generateTrivialTranslationRouterFiles modulePrefix moduleName localeName translations =
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
                    [ [ moduleDeclaration directory name
                      , [ importExposing [ "Static", "Text" ] [] "Text"
                        , localeName
                            |> String.toSentenceCase
                            |> qualifiedImport (directory ++ [ name ])
                        ]
                            |> joinLines
                      ]
                        |> joinLinesWith 1
                    , keysCodes
                        |> List.map
                            (\( key, code ) ->
                                [ String.join " " [ key, ": ", code ]
                                , String.join " " [ key, " =" ]
                                , String.join "." [ String.toSentenceCase localeName, key ]
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
        [ moduleDeclaration modulePrefix "Locales"
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


moduleDeclaration : List String -> String -> String
moduleDeclaration modules name =
    [ "module"
    , modules
        ++ [ name ]
        |> String.join "."
    , "exposing (..)"
    ]
        |> String.join " "


import_ : List String -> String -> String
import_ modules name =
    [ "import"
    , modules
        ++ [ name ]
        |> String.join "."
    , "exposing (..)"
    ]
        |> String.join " "


importExposing : List String -> List String -> String -> String
importExposing exposed modules name =
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


qualifiedImport : List String -> String -> String
qualifiedImport modules name =
    [ "import"
    , modules
        ++ [ name ]
        |> String.join "."
    , "as"
    , String.toSentenceCase name
    , "exposing (..)"
    ]
        |> String.join " "
