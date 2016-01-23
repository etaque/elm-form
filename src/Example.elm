module Example where

import Task exposing (Task)
import String

import StartApp
import Effects exposing (Effects)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
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

type alias Model =
  { form : Form CustomError User }


-- Init

init : (Model, Effects Action)
init =
  ({ form = Form.initial validation }, Effects.none)

validation : Validation CustomError User
validation =
  form5 User
    ("name" := (string |> map String.trim |> pipeTo nonEmpty))
    ("age" ?= (int `andThen` (minInt 18) |> customError Ooops))
    ("admin" := bool |> defaultValue False)
    ("role" := string)
    ("profile" := form2 Profile
      ("foo" := string)
      ("bar" := string))


-- Action

type Action
  = NoOp
  | FormAction Form.Action


-- Update

mailbox : Signal.Mailbox Action
mailbox =
  Signal.mailbox NoOp

update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)


-- View

view : Signal.Address Action -> Model -> Html
view address {form} =
  let
    formAddress = Signal.forwardTo mailbox.address FormAction
    inputGroup name builder =
      div
        [ style [ ("margin", "10px 0") ] ]
        [ label [] [ text name ]
        , br [] []
        , builder name form formAddress []
        , case Input.liveErrorAt name form of
            Just error ->
              div
                [ style [("color", "red"), ("margin-top", "5px")] ]
                [ text (toString error) ]
            Nothing ->
              text ""
        ]
  in
    div [ style [ ("margin", "50px auto"), ("width", "400px")] ]
      [ inputGroup "name" Input.textInput
      , inputGroup "age" Input.textInput
      , inputGroup "admin" Input.checkboxInput
      , inputGroup "role" <|
          Input.selectInput [ ("", "--"), ("a", "Option A"), ("b", "Option B") ]
      , inputGroup "profile.foo" Input.textInput
      , inputGroup "profile.bar" Input.textInput
      , button
          [ onClick formAddress Form.submit ]
          [ text "Submit" ]
      , hr [] []
      , text (toString (Form.getOutput form))
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


