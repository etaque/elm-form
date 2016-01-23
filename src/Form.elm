module Form
  ( Action, Form, WithForm, Setup
  , updateStringAt, updateBoolAt, validate
  , initial, wrappedUpdate, update
  , getFieldAt, getBoolAt, getStringAt, setFieldAt, getErrorAt
  , isDirty, isSubmitted
  ) where

import Dict exposing (Dict)
import Result
import Effects exposing (Effects, Never, none)
import Task exposing (Task)
import Response exposing (..)
import String

import Form.Error as Error exposing (..)
import Form.Field as Field exposing (..)
import Form.Validate as Validate


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
  { field : Field
  , submitted : Bool
  , errors : Error customError
  , setup : Setup customError target action
  }


{-| Form setup:
* how to validate the form
* initial field
* which action to call on success, taking validated object as parameter
* which action to call on error
-}
type alias Setup customError target action =
  { validation : Validate.Validation customError target
  , initialFields : Dict String Field
  , onOk : target -> action
  , onErr : action
  }


{-| Initial field of the form. -}
initial : Setup e target action -> Form e target action
initial setup =
  F <|
    { field = Group setup.initialFields
    , submitted = False
    , errors = GroupErrors Dict.empty
    , setup = setup
    }


{-| Form action -}
type Action
  = NoOp
  | UpdateField String Field
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

    UpdateField name field ->
      let
        newField = setFieldAt name field (F model)
        newModel = { model | field = newField}
      in
        res (F newModel) none

    Validate ->
      case model.setup.validation model.field of

        Ok field ->
          let
            newModel = { model
              | errors = GroupErrors Dict.empty
              , submitted = True
              }
            t = Task.succeed (model.setup.onOk field)
          in
            taskRes (F newModel) t

        Err error ->
          let
            newModel = { model
              | errors = error
              , submitted = True
              }
            t = Task.succeed model.setup.onErr
          in
            taskRes (F newModel) t


{-| -}
getFieldAt : String -> Form e t a -> Maybe Field
getFieldAt qualifiedName (F model) =
  let
    walkPath name maybeField =
      case maybeField of
        Just field ->
          Field.getAt name field
        Nothing ->
          Nothing
  in
    List.foldl walkPath (Just model.field) (String.split "." qualifiedName)


{-| -}
getStringAt : String -> Form e t a -> Maybe String
getStringAt name form =
  getFieldAt name form `Maybe.andThen` getString


{-| -}
getBoolAt : String -> Form e t a -> Maybe Bool
getBoolAt name form =
  getFieldAt name form `Maybe.andThen` getBool


{-| -}
setFieldAt : String -> Field -> Form e t a -> Field
setFieldAt qualifiedName field (F model) =
  let
    walkPath path maybeNode =
      case path of
        name :: rest ->
          let
            node = Maybe.withDefault (Group Dict.empty) maybeNode
            childField = walkPath rest (Field.getAt name node)
          in
            merge (Group (Dict.fromList [ (name, childField) ])) node
        [] ->
          field
  in
    walkPath (String.split "." qualifiedName) (Just model.field)


{-| -}
getErrorAt : String -> Form e target action -> Maybe (Error e)
getErrorAt qualifiedName (F model) =
  let
    walkPath name maybeError =
      case maybeError of
        Just field ->
          Error.getAt name field
        Nothing ->
          Just EmptyError
  in
    List.foldl walkPath (Just model.errors) (String.split "." qualifiedName)


isDirty : String -> Form e t a -> Bool
isDirty qualifiedName (F model) =
  -- TODO
  False


isSubmitted : Form e t a -> Bool
isSubmitted (F model) =
  model.submitted


{-| Private. -}
merge : Field -> Field -> Field
merge v1 v2 =
  case (v1, v2) of
    (Group g1, Group g2) ->
      Group (Dict.union g1 g2)
    _ ->
      v1

