# Elm SimpleForm

HTML live forms builders and validation for Elm. [![Build Status](https://travis-ci.org/etaque/elm-simple-form.svg?branch=master)](https://travis-ci.org/etaque/elm-simple-form)

    elm package install etaque/elm-simple-form


## Features

* Validation API similar to `Json.Decode` with the standard `map`, `andThen`, etc: you either get the desired output value or all field errors
* HTML inputs helpers with pre-wired handlers for live validation
* Suite of basic validations, with a way to add your own
* Unlimited fields! See `apply` function, similar to Json.Extra
* Nested fields for record composition (`foo.bar.baz`)

[See complete example here](http://etaque.github.io/elm-simple-form/example/) ([source code](https://github.com/etaque/elm-simple-form/tree/master/example)).

Infix operators have been splitted to a [dedicated package](https://github.com/etaque/elm-simple-form-infix).


## Basic usage


```elm
module Main exposing (..)

import Html.App as Html
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


-- Add form to your model and msgs

type alias Model =
  { form : Form () Foo }

type Msg =
  NoOp | FormMsg Form.Msg


-- Setup form validation

init : (Model, Cmd Msg)
init =
  ({ form = Form.initial [] validate }, Cmd.none)


validate : Validation () Foo
validate =
  form2 Foo
    (get "bar" email)
    (get "baz" bool)


-- Forward form msgs to Form.update

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({form} as model) =
  case msg of

    NoOp ->
      (model, Cmd.none)

    FormMsg formMsg ->
      ({ model | form = Form.update formMsg form}, Cmd.none)


-- Render form with Input helpers

view : Model -> Html Msg
view {form} =
  Html.map FormMsg (formView form)

formView : Form () Foo -> Html Form.Msg
formView form =
  let
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
      , Input.textInput bar []
      , errorFor bar

      , label []
          [ Input.checkboxInput baz []
          , text "Baz"
          ]
      , errorFor baz

      , button
          [ onClick Form.Submit ]
          [ text "Submit" ]
      ]


app = Html.program
  { init = init
  , update = update
  , view = view
  , subscriptions = \_ -> Sub.none
  }
```


## Advanced usage

### Custom inputs

 * For rendering, `Form.getFieldAsString`/`Bool` provides a `FieldState` record with all required fields (see package doc).

 * For event handling, see all field related messages in `Form.Msg` type.

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
Input.textInput (Form.getFieldAsString "options.foo" form) []
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
button [ onClick (Form.Reset initialFields) ] [ text "Reset" ]
```

*Note:* To have programmatically control over any `input[type=text]`/`textarea` value, like reseting or changing the value, you must set the `value` attribute with `Maybe.withDefault "" state.value`, as seen [here](https://github.com/etaque/elm-simple-form/pull/57/files#diff-bfb877e82b2c89b329fcda943a258611R50). There's a downside of doing this: if the user types too fast, the caret can go crazy.

More info: https://github.com/evancz/elm-html/pull/81#issuecomment-145676200


### Custom errors

```elm
type LocalError = Fatal | NotSoBad

validate : Validate LocalError Foo
validate =
  (get "foo" (string |> customError Fatal))

-- creates `Form.Error.CustomError Fatal`
```


### Async validation

This package doesn't provide anything special for async validation, but doesn't prevent you to do that neither. As field values are accessible from `update` with `Form.getStringAt/getBoolAt`, you can process them as you need, trigger effects like an HTTP request, and then add any errors to the view by yourself.

Another way would be to enable dynamic validation reload, to make it dependant of an effect, as it's part of the form state. Please ping me if this feature would be useful to you.
