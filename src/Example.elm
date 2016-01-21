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
  , age : Maybe Int
  , admin : Bool
  , role : String
  , profile : Profile
  }

type alias Profile =
  { foo : String
  , bar : String
  }

type CustomError = Yay | Ooops

type alias Model = WithForm CustomError User Action
  { user : Maybe User }

init : (Model, Effects Action)
init =
  ({ form = Form.initial formSetup, user = Nothing }, Effects.none)

formSetup : Form.Setup CustomError User Action
formSetup =
  { validation = form5 User
      ("name" := (trim string `andThen` nonEmpty))
      ("age" ?= (int `andThen` (minInt 18) |> customError Ooops))
      ("admin" := bool)
      ("role" := string)
      ("profile" := (form2 Profile ("foo" := string) ("bar" := string)))
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
      , fieldGroup (selectInput [("a", "Sorcier"), ("b", "Magicien")]) "role"
      , fieldGroup textInput "profile.foo"
      , fieldGroup textInput "profile.bar"
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


