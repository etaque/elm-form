module Form
  ( Action, Form, WithForm, Setup
  , updateStringAt, updateBoolAt, validate
  , initial, wrappedUpdate, update
  , getFieldAt, getBoolAt, getStringAt
  , setFieldAt
  , getErrorAt
  ) where

import Dict exposing (Dict)
import Result
import Effects exposing (Effects, Never, none)
import Task exposing (Task)
import Response exposing (..)
import String

import Form.Core as Core exposing (..)
import Form.Error as Error exposing (..)
import Form.Value as Value exposing (..)


{-| Form to embed in your model, with type parameters being:
* your custom error type,
* the type that should be build by the form,
* the type of the action to call on error/success
-}
type Form customError target action =
  F (Model customError target action)


{-| A record wrapper to embed form on `form` field. -}
type alias WithForm customError target action model =
  { model | form : Form customError target action }


{-| Private -}
type alias Model customError target action =
  { value : Value
  , errors : ValidationError customError
  , setup : Setup customError target action
  }



{-| Form setup:
* how to validate the form
* initial value
* which action to call on success, taking validated object as parameter
* which action to call on error
-}
type alias Setup customError target action =
  Core.Setup customError target action


{-| Form action -}
type Action
  = NoOp
  | UpdateField String Value
  | Validate


updateStringAt : String -> String -> Action
updateStringAt name s =
  UpdateField name (Text s)


updateBoolAt : String -> Bool -> Action
updateBoolAt name b =
  UpdateField name (Check b)


validate : Action
validate =
  Validate

{-| Initial value of the form. -}
initial : Setup e target action -> Form e target action
initial setup =
  F { value = Group setup.initialFields, errors = GroupErrors Dict.empty, setup = setup }


{-| Update for `WithForm` usage. -}
wrappedUpdate : (Action -> action) -> Action -> WithForm e target action model -> (WithForm e target action model, Effects action)
wrappedUpdate actionWrapper action model =
  update actionWrapper action model.form
    |> mapModel (\form -> { model | form = form })


{-| Update for direct usage. -}
update : (Action -> action) -> Action -> Form e target action -> (Form e target action, Effects action)
update actionWrapper action (F model) =
  case action of

    NoOp ->
      res (F model) none

    UpdateField name value ->
      let
        newValue = setFieldAt name value (F model)
        newModel = { model | value = newValue}
      in
        res (F newModel) none

    Validate ->
      case model.setup.validation model.value of

        Ok value ->
          let
            newModel = { model | errors = GroupErrors Dict.empty }
            t = Task.succeed (model.setup.onOk value)
          in
            taskRes (F newModel) t

        Err formErrors ->
          let
            newModel = { model | errors = formErrors }
            t = Task.succeed model.setup.onErr
          in
            taskRes (F newModel) t




getFieldAt : String -> Form e t a -> Maybe Value
getFieldAt qualifiedName (F model) =
  let
    walkPath name maybeValue =
      case maybeValue of
        Just value ->
          getAt name value
        Nothing ->
          Nothing
  in
    List.foldl walkPath (Just model.value) (String.split "." qualifiedName)


getStringAt : String -> Form e t a -> Maybe String
getStringAt name form =
  getFieldAt name form `Maybe.andThen` getString


getBoolAt : String -> Form e t a -> Maybe Bool
getBoolAt name form =
  getFieldAt name form `Maybe.andThen` getBool


setFieldAt : String -> Value -> Form e t a -> Value
setFieldAt qualifiedName value (F model) =
  let
    walkPath path maybeNode =
      case path of
        name :: rest ->
          let
            node = Maybe.withDefault (Group Dict.empty) maybeNode
            childValue = walkPath rest (getAt name node)
          in
            merge (Group (Dict.fromList [ (name, childValue) ])) node
        [] ->
          value
  in
    walkPath (String.split "." qualifiedName) (Just model.value)


{-| Private. -}
merge : Value -> Value -> Value
merge v1 v2 =
  case (v1, v2) of
    (Group g1, Group g2) ->
      Group (Dict.union g1 g2)
    -- (Group _, _) ->
    --   v1
    -- (_, Group _) ->
    --   v2
    _ ->
      v1


getErrorAt : String -> Form e target action -> Maybe (Error e)
getErrorAt name (F form) =
  case form.errors of
    GroupErrors groupErrors ->
      case Dict.get name groupErrors of
        Just ve ->
          case ve of
            ValueError e -> Just e
            GroupErrors _ -> Nothing
        Nothing ->
          Nothing
    _ ->
      Nothing
