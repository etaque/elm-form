module Form
  ( Action, Form, FieldState
  , initial, update
  , getFieldAsString, getFieldAsBool
  -- , getBoolAt, getStringAt
  , getErrors --, getErrorAt
  , isSubmitted --, isDirtyAt, isVisitedAt
  , getOutput
  , validate, submit, reset
  , updateTextField, updateSelectField, updateCheckField, updateRadioField
  ) where

{-| Simple forms made easy, for Elm.

# Types
@docs Action, Form, FieldState

# Init/update lifecyle
@docs initial, update

# Field state accessors
@docs getFieldAsString, getFieldAsBool

# Global state accessors
@docs isSubmitted, getErrors, getOutput

# Field actions
@docs updateTextField, updateSelectField, updateCheckField, updateRadioField

# Global actions
@docs validate, submit, reset
-}


import Dict exposing (Dict)
import Result
import String
import Set exposing (Set)
import String

import Form.Error as Error exposing (..)
import Form.Field as Field exposing (..)
import Form.Validate as Validate exposing (Validation)


{-| Form to embed in your model, with type parameters being a custom error type to extend
built-in errors (set to `()` if you don't need it), and the type of the form output.
-}
type Form customError output =
  F (Model customError output)


{-| Private -}
type alias Model customError output =
  { fields : Field
  , dirtyFields : Set String
  , visitedFields : Set String
  , isSubmitted : Bool
  , output : Maybe output
  , errors : Error customError
  , validation : Validation customError output
  }


{-| Initial form state. See `Form.Field` for initial fields, and `Form.Validate` for validation. -}
initial : List (String, Field) -> Validation e output -> Form e output
initial initialFields validation =
  F <|
    { fields = group initialFields
    , dirtyFields = Set.empty
    , visitedFields = Set.empty
    , isSubmitted = False
    , output = Nothing
    , errors = GroupErrors Dict.empty
    , validation = validation
    }


{-| Field state containing all necessary data for view and update. -}
type alias FieldState e a =
  { path : String
  , value : Maybe a
  , error : Maybe (Error e)
  , liveError : Maybe (Error e)
  , isDirty : Bool
  , isVisited : Bool
  }


{-| Get field state at path, with value as a `String`. -}
getFieldAsString : String -> Form e o -> FieldState e String
getFieldAsString =
  getField getStringAt


{-| Get field state at path, with value as a `Bool`. -}
getFieldAsBool : String -> Form e o -> FieldState e Bool
getFieldAsBool =
  getField getBoolAt


getField : (String -> Form e o -> Maybe a) -> String -> Form e o -> FieldState e a
getField getValue path form =
  { path = path
  , value = getValue path form
  , error = getErrorAt path form
  , liveError = getLiveErrorAt path form
  , isDirty = isDirtyAt path form
  , isVisited = isVisitedAt path form
  }



{-| Form action -}
type Action
  = NoOp
  | UpdateField String Field
  | Validate
  | Submit
  | Reset (List (String, Field))


{-| Action to update the content of a text input at the given qualified path. -}
updateTextField : String -> String -> Action
updateTextField name s =
  UpdateField name (Text s)


{-| Action to update the state of a select input at the given qualified path. -}
updateSelectField : String -> String -> Action
updateSelectField =
  updateTextField


{-| Action to update the state of a radio input at the given qualified path. -}
updateRadioField : String -> String -> Action
updateRadioField =
  updateTextField


{-| Action to update the state of a chekbox input at the given qualified path. -}
updateCheckField : String -> Bool -> Action
updateCheckField name b =
  UpdateField name (Check b)


{-| Action to trigger validation of the form. -}
validate : Action
validate =
  Validate


{-| Action to submit the form. -}
submit : Action
submit =
  Submit


{-| Action to reset the form with the given fields. -}
reset : List (String, Field) -> Action
reset =
  Reset


{-| Update form state with the given action. -}
update : Action -> Form e output -> Form e output
update action (F model) =
  case action of

    NoOp ->
      F model

    UpdateField name field ->
      let
        newFields = setFieldAt name field (F model)
        newDirtyFields = Set.insert name model.dirtyFields
        newVisitedFields = Set.insert name model.visitedFields
        newModel = { model
          | fields = newFields
          , dirtyFields = newDirtyFields
          , visitedFields = newDirtyFields
          }
      in
        F newModel

    Validate ->
      F (updateValidate model)

    Submit ->
      let
        validatedModel = updateValidate model
      in
        F { validatedModel | isSubmitted = True }

    Reset fields ->
      let
        newModel = { model
          | fields = group fields
          , dirtyFields = Set.empty
          , visitedFields = Set.empty
          }
      in
        F newModel


{-| Private. -}
updateValidate : Model e o -> Model e o
updateValidate model =
  case model.validation model.fields of
    Ok output ->
      { model
        | errors = GroupErrors Dict.empty
        , dirtyFields = Set.empty
        , output = Just output
        }
    Err error ->
      { model
        | errors = error
        , dirtyFields = Set.empty
        , output = Nothing
        }


{-| Private. -}
getFieldAt : String -> Form e o -> Maybe Field
getFieldAt qualifiedName (F model) =
  let
    walkPath name maybeField =
      case maybeField of
        Just field ->
          Field.at name field
        Nothing ->
          Nothing
  in
    List.foldl walkPath (Just model.fields) (String.split "." qualifiedName)


{-| Access string value at given qualified path. -}
getStringAt : String -> Form e o -> Maybe String
getStringAt name form =
  getFieldAt name form `Maybe.andThen` asString


{-| Access boolean value at given qualified path. -}
getBoolAt : String -> Form e o -> Maybe Bool
getBoolAt name form =
  getFieldAt name form `Maybe.andThen` asBool


{-| Private. -}
setFieldAt : String -> Field -> Form e o -> Field
setFieldAt qualifiedName field (F model) =
  let
    walkPath path maybeNode =
      case path of
        name :: rest ->
          let
            node = Maybe.withDefault (Group Dict.empty) maybeNode
            childField = walkPath rest (Field.at name node)
          in
            merge (Group (Dict.fromList [ (name, childField) ])) node
        [] ->
          field
  in
    walkPath (String.split "." qualifiedName) (Just model.fields)


{-| Get list of errors on qualified paths. -}
getErrors : Form e o -> List (String, Error e)
getErrors (F model) =
  let
    mapGroupItem path (name, error) =
      walkTree (path ++ [ name] ) error
    walkTree path node =
      case node of
        GroupErrors errors ->
          List.concatMap (mapGroupItem path) (Dict.toList errors)
        _ ->
          [ (String.join "." path, node) ]
  in
    walkTree [] model.errors


{-| Get error at qualified path. -}
getErrorAt : String -> Form e o -> Maybe (Error e)
getErrorAt qualifiedName (F model) =
  let
    walkPath path maybeNode =
      case path of
        name :: rest ->
          case maybeNode of
            Just error ->
              case error of
                GroupErrors _ ->
                  walkPath rest (Error.getAt name error)
                _ ->
                  Just error
            Nothing ->
              Nothing
        [] ->
          maybeNode
  in
    walkPath (String.split "." qualifiedName) (Just model.errors)


getLiveErrorAt : String -> Form e o -> Maybe (Error e)
getLiveErrorAt name form =
  if isSubmitted form || (isVisitedAt name form && not (isDirtyAt name form))
    then getErrorAt name form
    else Nothing


{-| Get form output, in case of validation success. -}
getOutput : Form e o -> Maybe o
getOutput (F model) =
  model.output


{-| Get form submission state. Useful to show errors on unvisited fields. -}
isSubmitted : Form e o -> Bool
isSubmitted (F model) =
  model.isSubmitted


{-| Get input visited state, given qualified path. -}
isVisitedAt : String -> Form e o -> Bool
isVisitedAt qualifiedName (F model) =
  Set.member qualifiedName model.visitedFields


{-| Get input dirty state, given qualified path. -}
isDirtyAt : String -> Form e o -> Bool
isDirtyAt qualifiedName (F model) =
  Set.member qualifiedName model.dirtyFields


{-| Private. -}
merge : Field -> Field -> Field
merge v1 v2 =
  case (v1, v2) of
    (Group g1, Group g2) ->
      Group (Dict.union g1 g2)
    _ ->
      v1
