module Update where

import Effects exposing (Effects)
import Form exposing (Form)

import Model exposing (..)


init : (Model, Effects Action)
init =
  ({ form = Form.initial initialFields validate, userMaybe = Nothing }, Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action ({form} as model) =
  case action of

    NoOp ->
      (model, Effects.none)

    FormAction formAction ->
      ({ model | form = Form.update formAction form}, Effects.none)

    SubmitUser user ->
      ({ model | userMaybe = Just user }, Effects.none)


