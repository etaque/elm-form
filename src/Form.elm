module Form where

import Dict exposing (Dict)
import Result
import Effects exposing (Effects, Never, none)
import Task exposing (Task)
import Response exposing (..)

import Form.Model as Model exposing (..)


type alias Form e target action = Model.Form e target action
type alias WithForm e target action model = Model.WithForm e target action model
type alias Setup e target action = Model.Setup e target action
type alias Action = Model.Action


initial : Setup e target action -> Form e target action
initial setup =
  { value = Group setup.initialFields, errors = GroupErrors Dict.empty, setup = setup }


modelUpdate : (Action -> action) -> Action -> WithForm e target action model -> (WithForm e target action model, Effects action)
modelUpdate actionWrapper action model =
  formUpdate actionWrapper action model.form
    |> mapModel (\form -> { model | form = form })


formUpdate : (Action -> action) -> Action -> Form e target action -> (Form e target action, Effects action)
formUpdate actionWrapper action ({setup} as form) =
  case action of

    NoOp ->
      res form none

    UpdateField name value ->
      let
        newValue = setFieldAt name value form
        -- newErrors = Dict.insert name [] form.errors
        newForm =
          { form
            | value = newValue
            -- , errors = newErrors
          }
      in
        res newForm none

    Validate ->
      case setup.validation form.value of

        Ok value ->
          let
            newForm = { form | errors = GroupErrors Dict.empty }
            t = Task.succeed (setup.onOk value)
          in
            taskRes newForm t

        Err formErrors ->
          let
            newForm = Debug.log "withErrors" { form | errors = formErrors }
            t = Task.succeed setup.onErr
          in
            taskRes newForm t


emptyFields : Fields
emptyFields =
  Dict.empty
