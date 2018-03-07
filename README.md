# Translation Runner

This program generates Elm modules with Translations from JSON-files. The
generated code uses
[`kirchner/elm-translation`](https://github.com/kirchner/elm-translation) and
by default the helpers from
[`kirchner/elm-cldr`](https://github.com/kirchner/elm-cldr).


## Usage

In the directory containing your `elm-package.json` run

```
$ elm-translation init
```

It will look inside `./translations/` for JSON-files like `en.json`, ...
(assuming that these contain your translations) and then generate an
`elm-translation.json` configuration file. You may want to take a look at it
and adjust it to your needs.

Your translation JSON-files should look something like this:

```json
// ./translations/en.json
{
    "greeting": "Good morning!",
    "question": "Hello, {name}! Good to have you back. One question: is {email} still your email address?",
    "feature": {
        "title": "Our Newest Feature"
    }
}
```

and

```json
// ./translations/de.json
{
    "greeting": "Guten morgen!",
    "question": "Hallo, {name}! Schön, dass Du zurück bist. Eine Frage: ist {email} immer noch deine aktuelle Email-Adresse?",
    "feature": {
        "title": "Unser neuestes Feature"
    }
}
```

To generate Elm modules from these you have to run

```
$ elm-translation generate-elm
```

If everything goes well, you can use these in your application like so

```elm
import Html exposing (Html)
import Locales exposing (Locale)
import Translation exposing (asString, asStringWith)
import Translations as T
import Translations.Feature as TFeature

view : Locale -> String -> String -> Html msg
view locale name email =
    Html.div []
        [ T.greeting locale
            |> asString
            |> Html.text
        , T.question locale
            |> asStringWith
                { name = name
                , email = email
                }
            |> Html.text
        , TFeature.title locale
            |> asString
        ]
```

Note, that you will have to add `kirchner/elm-translation` and
`kirchner/elm-cldr` to your elm dependencies:

```
$ elm-package install kirchner/elm-translation
$ elm-package install kirchner/elm-cldr
```
