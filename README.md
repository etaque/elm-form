# elm-simple-form

Simple forms made easy, for Elm.

__Work in progress__

## Features

* Validation API similar to `Json.Decode` with the standard `map`, `andThen`, etc
* You get either the desired output value or all field errors
* HTML inputs helpers for live validation
* Suite of basic validations, with a way to add your own
* Unlimited fields! See `:+`
* Nested fields for record composition

[See example here.](./example/Main.elm)

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


## Usage


### Model

Say you have a `User` record for which you want to create a form:

```elm
type alias User =
  { name : String
  , email : String
  , age : Int
  }
```

Then you can add the `form` field to your model (first type parameter is for custom error type, here with `()` we're ignoring it):

```elm
import Form exposing (Form)

type alias Model =
  { form : Form () User }
```

Let's add an action case for the form:

```elm
type Action
  = NoOp
  | FormAction Form.Action
  | SubmitUser User
```


### Init

Next is form initialisation. This will require an initial value for fields (`[]`, we'll cover that later), and a validation process.

```elm
import Form.Validate as Validate exposing (..)

initialForm : Form
initialForm = 
  Form.initial [] validation

validation : Validation () User
validation =
  form3 User
    ("name" := (string |> map String.trim) `andThen` nonEmpty)
    ("email" := string `andThen` email)
    ("age" := int `andThen` (minInt 18))
```

So what's going here?

* We want to validate a form for a record with 3 fields
* `name` is a string that must not be empty after trimmming
* `email` must be a valid email
* `age` is an int that must be over 18


We are now ready for ~~ignition~~ initialization:

```elm
init : (Model, Effects Action)
init =
  ({ form = initialForm }, Effects.none)
```


### Update

This package sells simplicity, you won't be disappointed:

```elm
update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)

    SubmitUser user ->
      -- Successfully validated user! post it to your server, update your model... 
      (model, Effects.none)
```

That's all.


### View

SimpleForm provides a few helpers enabled for live validation in `Form.Input` module:

* `textInput`
* `selectInput`
* `checkboxInput`
* `radioInput`

Here is a way to arrange it, the idea is to create your own field decorators.

```elm
import Form.Input as Input

view : Signal.Address Action -> Model -> Html
view address {form} =
  let
    formAddress = Signal.forwardTo address FormAction
    inputGroup name builder =
      div
        []
        [ label [] [ text name ]
        , builder name form formAddress []
        , case Input.liveErrorAt name form of
            Just error ->
              div
                [ class "error" ]
                [ text (toString error) ]
            Nothing ->
              text ""
        ]
    submitOnClick =
      case Form.getOutput form of
        Just user ->
          onClick address (SubmitUser user)
        Nothing ->
          onClick formAddress Form.submit
  in
    div []
      [ inputGroup "name" Input.textInput
      , inputGroup "email" Input.textInput
      , inputGroup "age" Input.textInput
      , button
          [ submitOnClick ]
          [ text "Submit" ]
      ]
```

But you're not entitled to use `Form.Input`. One can build inputs from scratch with the following primitives in `Form`, see source code.
