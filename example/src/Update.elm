module Update where

import Task exposing (Task)
import Effects exposing (Effects)
import String

import Form exposing (Form)
import Form.Validate as Validate exposing (..)

import Model exposing (..)


init : (Model, Effects Action)
init =
  ({ form = Form.initial initialFields validation, userMaybe = Nothing }, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)

    SubmitUser user ->
      ({ model | userMaybe = Just user }, Effects.none)


