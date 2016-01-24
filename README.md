# elm-simple-form

Simple forms made easy, for Elm.

__Work in progress__

## Features

* Validation API similar to `Json.Decode` with the standard `map`, `andThen`, etc
* You get either the desired output value or all field errors
* HTML inputs helpers for live validation
* Suite of frequent validations, with a way to add your owns

```elm
form6 User
  ("name" := (string |> map String.trim) `andThen` nonEmpty)
  ("email" := string `andThen` email)
  ("age" := int `andThen` (minInt 18) |> customError Ooops)
  ("admin" := bool |> defaultValue False)
  ("role" ?= string)
  ("profile" := form2 Profile
    ("foo" := string `andThen` (includedIn foos))
    ("bar" := string))
```

