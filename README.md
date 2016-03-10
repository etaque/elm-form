# Elm SimpleForm

Simple forms made easy, for Elm.

    elm package install etaque/elm-simple-form


## Features

* Validation API similar to `Json.Decode` with the standard `map`, `andThen`, etc: you either get the desired output value or all field errors
* HTML inputs helpers with pre-wired handlers for live validation
* Suite of basic validations, with a way to add your own
* Unlimited fields! See `apply` function, similar to Json.Extra
* Nested fields for record composition (`foo.bar.baz`)

[See complete example here](http://etaque.github.io/elm-simple-form/example/) ([source code](./example/)).

Infix operators have been splitted to a [dedicated package](https://github.com/etaque/elm-simple-form-infix).


## Basic usage


```elm
module Main where

import StartApp
import Task exposing (Task)
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Input


-- your expected form output

type alias Foo =
  { bar : String
  , baz : Bool
  }


-- Add form to your model and actions

type alias Model =
  { form : Form () Foo }

type Action =
  NoOp | FormAction Form.Action


-- Setup form validation

init : (Model, Effects Action)
init =
  ({ form = Form.initial [] validate }, Effects.none)


validate : Validation () Foo
validate =
  form2 Foo
    (get "bar" email)
    (get "baz" bool)


-- Forward form actions to Form.update

update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)


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
          [ onClick formAddress Form.OnSubmit ]
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

### Custom inputs

 * For rendering, `Form.getFieldAsString`/`Bool` provides a `FieldState` record with all required fields (see package doc).

 * For event handling, see all field related actions in `Form.Action` type.

Overall, having a look at current [helpers source code](https://github.com/etaque/elm-simple-form/blob/master/src/Form/Input.elm) should give you a good idea of the thing.

### Incremental validation

Similar to what Json.Extra provides. Use `Form.apply`, or the `|:` infix version from [infix package](https://github.com/etaque/elm-simple-form-infix):

```elm
Form.succeed Player
  `apply` (get "email" (string `andThen` email))
  `apply` (get "power" int)
```

### Nested records

* Validation:

```elm
validate =
  form2 Player
    (get "email" (string `andThen` email))
    (get "power" (int `andThen` (minInt 0)))
    (get "options"
      (form2 Options
        (get "foo" string)
        (get "bar" string)
      )
    )
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
  [ ("power", Field.Text "10")
  , ("options", Field.group
      [ ("foo", Field.Text "blah")
      , ("bar", Field.Text "meh")
      ]
    )
  ]

initialForm : Form
initialForm =
  Form.initial initialFields validate
```

See `Form.Field` type for more options.

* On demand:

```elm
button [ onClick formAddress (Form.reset initialFields) ] [ text "Reset" ]
```


### Custom errors

```elm
type LocalError = Fatal | NotSoBad

validate : Validate LocalError Foo
validate =
  (get "foo" (string |> customError Fatal))

-- creates `Form.Error.CustomError Fatal`
```


### Async validation

This package doesn't provide anything special for async validation, but doesn't prevent you to do that neither. As field values are accessible from `update` with `Form.getStringAt/getBoolAt`, you can proceed them as you need, trigger effects like an HTTP request, and then add any errors to the view by yourself.

Another way would be to enable dynamic validation reload, to make it dependant of an effect, as it's part of the form state. Please ping me if this feature would be useful to you.
