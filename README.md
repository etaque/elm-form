# Elm SimpleForm

Simple forms made easy, for Elm.

_Work in progress_

## Features

* Validation API similar to `Json.Decode` with the standard `map`, `andThen`, etc ; you either get the desired output value or all field errors
* HTML inputs helpers for live validation
* Suite of basic validations, with a way to add your own
* Unlimited fields! See `:+`
* Nested fields for record composition (`foo.bar.baz`)

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

[See complete example here.](./example/Main.elm)


## Basic usage

```elm
module Main where

import StartApp
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Input


type alias Player =
  { email : String
  , power : Int
  }


-- Add form to your model and actions

type alias Model =
  { form : Form () Player }

type Action
  = NoOp
  | FormAction Form.Action


-- Setup form validation

init : (Model, Effects Action)
init =
  ({ form = Form.initial [] validation }, Effects.none)

validation : Validation () Player
validation =
  form2 Player
    ("email" := string `andThen` email)
    ("power" := int `andThen` (minInt 0))
    

-- Forward form actions to Form.update

update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)

    SubmitUser user ->
      ({ model | userMaybe = Just user }, Effects.none)


-- Render form with Input helpers

view : Signal.Address Action -> Model -> Html
view address {form} =
  let
    -- Address for Form events
    formAddress = Signal.forwardTo address FormAction

    -- error presenter
    errorFor name =
      case Input.liveErrorAt name form of
        Just error ->
          -- replace toString with your own translations
          div [ class "error" ] [ text (toString error) ] 
        Nothing ->
          text ""
  in
    div []
      [ label [] [ text "Email" ]
      , Input.textInput "email" form formAddress []
      , errorFor "email"
      
      , label [] [ text "Power" ]
      , Input.textInput "power" form formAddress []
      , errorFor "power"
      
      , button
          [ onClick formAddress Form.submit ]
          [ text "Submit" ]
      ]


-- Classic StartApp wiring

app = StartApp.start
  { init = init
  , update = update
  , view = view
  , inputs = [ ]
  }

main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks
```


## Advanced usage

### Incremental validation

Similar to what Json.Extra provides. Use `Form.apply`, or the `|:` infix version:

```elm
Form.succeed Player
  |: ("email" := string `andThen` email)
  |: ("power" := int)
```

### Nested records

* Validation:

```elm
validation =
  form2 Player
    ("email" := string `andThen` email)
    ("power" := int `andThen` (minInt 0))
    ("options" := form2 Options
      ("foo" := string)
      ("bar" := string))
```

* View:

```elm
Input.textInput "options.foo" form formAddress []
Input.liveErrorAt "options.foo" form
```

### Initial values and reset

* At form initialization:

```elm
import Form.Field as Field

initialFields : List (String, Field)
initialFields =
  [ ("power", Field.text "10")
  , ("options", Field.group
      [ ("foo", Field.text "blah")
      , ("bar", Field.text "meh")
      ]
    )
  ]

initialForm : Form
initialForm =
  Form.initial initialFields validation
```

See `Form.Field` functions for more options.

* On demand:

```elm
button [ onClick formAddress (Form.reset initialFields) ] [ text "Reset" ]
```


### Custom errors

```elm
type LocalError = Fatal | NotSoBad

validation : Validation LocalError Foo
validation =
  ("foo" := string |> customError Fatal)

-- creates `Form.Error.CustomError Fatal`
```
