module Example where

import Task exposing (Task)
import String
import Dict

import StartApp
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)

import Form exposing (Form, WithForm)
import Form.Validate as Validate exposing (..)
import Form.Input as Input exposing (..)


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


-- Init

init : (Model, Effects Action)
init =
  ({ form = Form.initial formSetup, user = Nothing }, Effects.none)

formSetup : Form.Setup CustomError User Action
formSetup =
  { validation = form5 User
      ("name" := (string |> map String.trim |> pipeTo nonEmpty))
      ("age" ?= (int `andThen` (minInt 18) |> customError Ooops))
      ("admin" := bool |> withDefault False)
      ("role" := string)
      ("profile" := form2 Profile
        ("foo" := string)
        ("bar" := string))
  , initialFields = Dict.empty
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
      Form.wrappedUpdate FormAction formAction model

    FormSuccess user ->
      ({ model | user = Just user }, Effects.none)


-- View

view : Signal.Address Action -> Model -> Html
view address model =
  let
    formAddress = Signal.forwardTo mailbox.address FormAction
    field name builder =
      div
        [ class "field", style [ ("margin", "10px 0") ] ]
        [ builder name model.form formAddress []
        , div
            [ style [("color", "red"), ("margin-top", "5px")] ]
            [ errorMessage model.form name toString ]
        ]
  in
    div [ style [ ("margin", "50px auto"), ("width", "400px")] ]
      [ field "name" textInput
      , field "age" textInput
      , field "admin" checkboxInput
      , field "role" (selectInput [("a", "Sorcier"), ("b", "Magicien")])

      , field "profile.foo" textInput
      , field "profile.bar" textInput

      , button
          [ validateOnClick formAddress ]
          [ text "Ok" ]
      , hr [] []
      , text (toString model.user)
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


