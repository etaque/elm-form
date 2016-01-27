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


type alias Foo =
  { bar : String
  , baz : Bool
  }


-- Add form to your model and actions

type alias Model =
  { form : Form () Foo }

type Action
  = NoOp
  | FormAction Form.Action


-- Setup form validation

init : (Model, Effects Action)
init =
  ({ form = Form.initial [] validate }, Effects.none)

validate : Validate () Form
validate =
  form2 Foo
    ("bar" := string `andThen` email)
    ("baz" := bool)
    

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
    errorFor field =
      case field.liveError of
        Just error ->
          -- replace toString with your own translations
          div [ class "error" ] [ text (toString error) ] 
        Nothing ->
          text ""
          
    -- fields states
    bar = Form.getFieldAsString "bar" form
    baz = Form.getFieldAsBool "baz" form
  in
    div []
      [ label [] [ text "Bar" ]
      , Input.textInput bar formAddress []
      , errorFor bar
      
      , label [] 
          [ Input.checkboxInput baz formAddress []
          , text "Baz"
          ]
      , errorFor baz
      
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
validate =
  form2 Player
    ("email" := string `andThen` email)
    ("power" := int `andThen` (minInt 0))
    ("options" := form2 Options
      ("foo" := string)
      ("bar" := string))
```

* View:

```elm
Input.textInput (Form.getFieldAsString "options.foo" form) formAddress []
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
  Form.initial initialFields validate
```

See `Form.Field` functions for more options.

* On demand:

```elm
button [ onClick formAddress (Form.reset initialFields) ] [ text "Reset" ]
```


### Custom errors

```elm
type LocalError = Fatal | NotSoBad

validate : Validate LocalError Foo
validate =
  ("foo" := string |> customError Fatal)

-- creates `Form.Error.CustomError Fatal`
```


### Async validation

This package doesn't provide anything special for async validation, but doesn't prevent you to do that neither. As field values are accessible from `update` with `Form.getStringAt/getBoolAt`, you can proceed them as you need, trigger effects like an HTTP request, and then add any errors to the view by yourself.

Another way would be to enable dynamic validation reload, to make it dependant of an effect, as it's part of the form state. Please ping me if this feature would be useful to you.
