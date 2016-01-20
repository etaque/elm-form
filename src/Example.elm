module Example where

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Effects exposing (Effects)
import Task exposing (Task)
import Result

import Form exposing (Form, WithForm)
import Form.Validate as Validate exposing (..)
import Form.View as FormView exposing (..)


-- Model

type alias User =
  { name : String
  , age : Int
  , admin : Bool
  }

type alias Model = WithForm User Action
  { user : Maybe User }

init : (Model, Effects Action)
init =
  ({ form = Form.initial formSetup, user = Nothing }, Effects.none)

formSetup : Form.Setup User Action
formSetup =
  { validation = form3 User
      ("name" := (trim string `andThen` nonEmpty))
      ("age" := (int `andThen` (minInt 0) |> customError [ "hey" ]))
      ("admin" := bool)
  , initialFields = Form.emptyFields
  , onOk = FormSuccess
  , onErr = NoOp
  }


-- Action

type Action
  = NoOp
  | FormAction Form.Action
  | FormSuccess User


-- Update

mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NoOp

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      Form.modelUpdate FormAction formAction model

    FormSuccess user ->
      ({ model | user = Just user }, Effects.none)


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    formAddress = Signal.forwardTo mailbox.address FormAction
    fieldGroup builder name =
      div
        [ class "field-group" ]
        [ builder name model.form formAddress []
        , errorsOn model.form name toString
        ]
  in
    div [ ]
      [ fieldGroup textInput "name"
      , fieldGroup textInput "age"
      , fieldGroup checkboxInput "admin"
      , button [ validateOnClick formAddress ] [ text "Ok" ]
      , div [] [ text (toString model.user) ]
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


