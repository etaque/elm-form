module Update exposing (..)

import Form exposing (Form)
import Model exposing (..)


init : (Model, Cmd Msg)
init =
  ({ form = Form.initial initialFields validate, userMaybe = Nothing }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg ({form} as model) =
  case msg of

    NoOp ->
      (model, Cmd.none)

    FormMsg formMsg ->
      ({ model | form = Form.update formMsg form}, Cmd.none)

    SubmitUser user ->
      ({ model | userMaybe = Just user }, Cmd.none)

