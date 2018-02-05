module Main exposing (main)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform exposing (programWithFlags)
import Ports
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
    , locales
        |> List.map (generateLocaleModules modulePrefix)
        |> Cmd.batch
    )


generateLocaleModules : String -> { locale : String, rawJson : String } -> Cmd msg
generateLocaleModules modulePrefix { locale, rawJson } =
    case Decode.decodeString directoryDecoder rawJson of
        Ok directory ->
            directory
                |> collectMessages []
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

        Err error ->
            Debug.crash error



---- COLLECT MESSAGES


type Directory
    = Directory (Dict String Directory)
    | Entry String


directoryDecoder : Decoder Directory
directoryDecoder =
    Decode.oneOf
        [ Decode.dict (Decode.lazy (\_ -> directoryDecoder))
            |> Decode.map Directory
        , Decode.string
            |> Decode.map Entry
        ]


collectMessages :
    List String
    -> Directory
    -> Dict (List String) (Dict String String)
collectMessages scope directory =
    case directory of
        Entry newMessage ->
            case scope of
                name :: moduleNames ->
                    Dict.singleton (List.reverse moduleNames) (Dict.singleton name newMessage)

                _ ->
                    Debug.crash "unnamed translation"

        Directory entries ->
            let
                merge dictA dictB =
                    Dict.merge
                        (\scope a collection -> Dict.insert scope a collection)
                        (\scope a b collection -> Dict.insert scope (Dict.union a b) collection)
                        (\scope b collection -> Dict.insert scope b collection)
                        dictA
                        dictB
                        Dict.empty
            in
            entries
                |> Dict.toList
                |> List.map
                    (\( name, entry ) ->
                        collectMessages (name :: scope) entry
                    )
                |> List.foldr merge Dict.empty



---- CODE GENERATION


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
    let
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
    in
    Translation.toElm cldrToArgType
        (String.camelize name)
        icuMessage


moduleName : List String -> String
moduleName =
    List.map (String.toSentenceCase >> String.camelize)
        >> String.join "."
