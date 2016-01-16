module Example where

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Effects exposing (Effects)
import Task exposing (Task)
import Result

import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.View as FormView exposing (..)


-- Model

type alias Stuff =
  { foo : String
  , bar : Maybe Int
  , baz : Bool
  }

type alias Model =
  { form : Form }

init : (Model, Effects Action)
init =
  ({ form = Form.emptyForm }, Effects.none)


-- Action

type Action
  = NoOp
  | FormAction Form.Action
  | SubmitForm Stuff


-- Update

mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NoOp

(:=) = get
(?=) = maybe

formSetup : Form.Setup Stuff Action
formSetup =
  { validate = Form.validate3 Stuff
      ("foo" := string)
      ("bar" ?= (int `andThen` (Validate.min 5)))
      ("baz" := bool)
  , address = mailbox.address
  , okHandler = SubmitForm
  , errHandler = NoOp
  }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case Debug.log "action" action of
    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      let
        (newForm, fx) = Form.update formSetup formAction model.form
      in
        ({ model | form = newForm }, Effects.map FormAction fx)

    SubmitForm stuff ->
      let
        _ = Debug.log "stuff" stuff
      in
        (model, Effects.none)


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    formAddress = Signal.forwardTo mailbox.address FormAction
    fieldGroup builder name =
      div
        [ class "field-group" ]
        [ textInput name model.form formAddress []
        , errorsOn model.form name toString
        ]
  in
    div [ ]
      [ fieldGroup textInput "foo"
      , fieldGroup textInput "bar"
      , fieldGroup checkboxInput "baz"
      , button [ validateOnClick formAddress ] [ text "Ok" ]
      ]


-- App

app = StartApp.start
  { init = init
  , update = update
  , view = view
  , inputs = [ mailbox.signal ]
  }

main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks


