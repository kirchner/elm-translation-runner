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
    , [ case collectTranslations locales of
            Ok byScope ->
                let
                    allLocales =
                        collectLocales byScope

                    translationsList =
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
                                            ( Module modules name
                                            , Dict.toList byKey
                                            )

                                        _ ->
                                            Debug.crash "should not happen!"
                                )

                    modules =
                        modulePrefix
                            |> String.split "."
                in
                [ translationsList
                    |> translationsRouterFiles modules allLocales
                    |> List.map writeFile
                    |> Cmd.batch
                , translationsList
                    |> translationsFiles modules allLocales
                    |> List.map writeFile
                    |> Cmd.batch
                , allLocales
                    |> localesFile modules
                    |> writeFile
                ]
                    |> Cmd.batch

            Err error ->
                Cmd.none
      ]
        |> Cmd.batch
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
      , content
            |> Encode.string
      )
    ]
        |> Encode.object
        |> Ports.writeModule



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
    List { locale : String, rawJson : String }
    -> Result String (Dict Scope (Dict Key (Dict Locale String)))
collectTranslations rawData =
    rawData
        |> List.map
            (\{ locale, rawJson } ->
                case Decode.decodeString directoryDecoder rawJson of
                    Ok (Directory entries) ->
                        Ok
                            ( locale
                            , translationsByScope entries
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


collectLocales : Dict Scope (Dict Key (Dict Locale String)) -> Set String
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
              , generateImportExposing [ "Node" ] [] "VirtualDom"
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
    -> Set Locale
    -> List ( Module, List ( Key, Dict Locale String ) )
    -> List File
translationsFiles modules locales translations =
    locales
        |> Set.toList
        |> List.map (translationsFilesForLocale modules translations)
        |> List.concat


translationsFilesForLocale :
    List String
    -> List ( Module, List ( Key, Dict Locale String ) )
    -> Locale
    -> List File
translationsFilesForLocale modules translations locale =
    translations
        |> List.map (uncurry (translationsFileForLocale modules locale))


translationsFileForLocale :
    List String
    -> Locale
    -> Module
    -> List ( Key, Dict Locale String )
    -> File
translationsFileForLocale modules locale (Module scope name) translations =
    let
        maybeTranslationsDict =
            translations
                |> List.foldl insertTranslation (Just Dict.empty)

        insertTranslation ( key, byLocale ) =
            Maybe.andThen
                (\dict ->
                    Dict.get locale byLocale
                        |> Maybe.map (flip (Dict.insert key) dict)
                )

        actualModules =
            (modules ++ scope ++ [ name ])
                |> List.map String.toSentenceCase

        actualModule =
            String.toSentenceCase locale
    in
    case maybeTranslationsDict of
        Just translationsDict ->
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
                , translationsDict
                    |> Dict.toList
                    |> List.filterMap (uncurry generateMessage)
                    |> joinLinesWith 2
                ]
                    |> joinLinesWith 2
            }

        Nothing ->
            Debug.crash "TODO"


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
