module Generate
    exposing
        ( importExposing
        , import_
        , indent
        , joinLines
        , joinLinesWith
        , moduleDeclaration
        , qualifiedImport
        )

import String.Extra as String


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
