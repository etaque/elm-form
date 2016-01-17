module Form where

import Dict exposing (Dict)
import Result
import Effects exposing (Effects, Never, none)
import Task exposing (Task)
import Response exposing (..)

import Form.Model as Model exposing (..)


type alias Form = Model.Form
type alias Setup value action = Model.Setup value action
type alias Action = Model.Action


emptyForm : Form
emptyForm =
  Dict.empty


update : Setup v a -> Action -> Form -> (Form, Effects Action)
update setup action form =
  case action of

    NoOp ->
      (form, none)

    UpdateField name value ->
      let
        field : Field
        field = { value = value, errors = [] }
        newForm = Dict.insert name field form
      in
        (newForm, none)

    Validate ->
      case setup.validate form of
        Ok value ->
          Signal.send setup.address (setup.okHandler value)
            |> taskRes form
            |> mapEffects (\_ -> NoOp)
        Err formErrors ->
          Signal.send setup.address setup.errHandler
            |> taskRes (addErrorsToForm formErrors form)
            |> mapEffects (\_ -> NoOp)


addErrorsToForm : Dict String (List FieldError) -> Form -> Form
addErrorsToForm formErrors form =
  Dict.toList formErrors
    |> List.foldl addErrorsToField form


addErrorsToField : (String, List FieldError) -> Form -> Form
addErrorsToField (name, errors) form =
  if Dict.member name form then
    Dict.update name (Maybe.map (\f -> { f | errors = errors })) form
  else
    Dict.insert name { value = EmptyValue, errors = errors } form


validate : FieldValidator v -> Form -> FormResult v
validate {name, validate} form =
  validate (getValue form name)
    |> Result.formatError (\e -> Dict.fromList [ (name, [ e ]) ])


validate2 : (a -> b -> value) -> FieldValidator a -> FieldValidator b -> Form -> FormResult value
validate2 f v1 v2 form =
  case (validate v1 form, validate v2 form) of
    (Ok a, Ok b) ->
      Ok (f a b)
    (r1, r2) ->
      Err (mergeFormErrors [ err r1, err r2 ])


validate3 : (a -> b -> c -> value) -> FieldValidator a -> FieldValidator b -> FieldValidator c -> Form -> FormResult value
validate3 f v1 v2 v3 form =
  case (validate v1 form, validate v2 form, validate v3 form) of
    (Ok a, Ok b, Ok c) ->
      Ok (f a b c)
    (r1, r2, r3) ->
      Err (mergeFormErrors [ err r1, err r2, err r3 ])


mergeFormErrors : List (Maybe FormErrors) -> FormErrors
mergeFormErrors errors =
  List.filterMap identity errors
    |> List.foldl (Dict.union) Dict.empty


err : Result e a -> Maybe e
err res =
  case res of
    Ok _ -> Nothing
    Err e -> Just e


getValue =
  Model.getValue

getErrors =
  Model.getErrors
