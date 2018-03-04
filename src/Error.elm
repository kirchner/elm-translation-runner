module Error exposing (print)

{-| Pretty print the `Error`'s produced by `Icu.parse : String -> Result
Error Message`.

@docs print

-}

import Html exposing (Html)
import Html.Attributes as Attributes
import List.Extra as List
import Parser exposing (..)


print : Error -> String
print ({ row, col, source, problem, context } as error) =
    let
        message =
            [ "I ran into a problem while parsing "
            , context
                |> List.head
                |> Maybe.map .description
                |> Maybe.map blueText
                |> Maybe.withDefault ""
            , context
                |> List.drop 1
                |> List.head
                |> Maybe.map .description
                |> Maybe.map (\desc -> " of " ++ desc)
                |> Maybe.withDefault ""
            , "."
            ]

        contextRow =
            context
                |> List.head
                |> Maybe.map .row
                |> Maybe.withDefault row

        contextCol =
            context
                |> List.head
                |> Maybe.map .col
                |> Maybe.withDefault col

        printLine y =
            if y > 0 then
                source
                    |> String.split "\n"
                    |> List.drop (y - 1)
                    |> List.head
                    |> Maybe.map
                        (\line ->
                            [ toString y ++ "| "
                            , -- TODO: proper multiline coloring
                              line
                            , "\n"
                            ]
                        )
                    |> Maybe.withDefault []
            else
                []

        code =
            [ printLine (row - 2)
            , printLine (row - 1)
            , printLine row
            , [ redText arrow ]
            ]
                |> List.concat

        arrow =
            String.repeat (col + 3) " " ++ "^"
    in
    [ message
    , [ "\n\n" ]
    , code
    , [ "\n" ]
    , printProblem problem
    , [ "\n" ]
    ]
        |> List.concat
        |> String.concat


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
        ExpectingKeyword keyword ->
            { collected | keywords = keyword :: collected.keywords }

        ExpectingSymbol symbol ->
            { collected | symbols = symbol :: collected.symbols }

        BadOneOf problems ->
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
        BadOneOf problems ->
            []

        BadInt ->
            [ greenText "a number" ]

        BadRepeat ->
            []

        ExpectingSymbol symbol ->
            [ "the symbol '"
            , greenText <|
                escapeSymbol symbol
            , "'"
            ]

        ExpectingKeyword keyword ->
            [ "the keyword '"
            , greenText keyword
            , "'"
            ]

        ExpectingVariable ->
            [ greenText "a variable" ]

        Fail message ->
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



---- VIEW HELPER


redText : String -> String
redText text =
    coloredText red text


greenText : String -> String
greenText text =
    coloredText green text


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


(=>) : a -> b -> ( a, b )
(=>) =
    (,)
