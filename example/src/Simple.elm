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
    ("bar" := email)
    ("baz" := bool)


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
