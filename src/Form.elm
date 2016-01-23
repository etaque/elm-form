module Form
  ( Action, Form
  , updateStringAt, updateBoolAt, validate, submit
  , initial, update
  , getFieldAt, getBoolAt, getStringAt, setFieldAt, getErrorAt
  , isSubmitted, isDirtyAt, isVisitedAt, getOutput
  ) where

import Dict exposing (Dict)
import Result
import String
import Set exposing (Set)

import Form.Error as Error exposing (..)
import Form.Field as Field exposing (..)
import Form.Validate as Validate exposing (Validation)


{-| Form to embed in your model, with type parameters being:
* your custom error type,
* the type that should be build by the form,
* the type of the action to call on error/success
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


{-| Initial form state. -}
initial : Validation e output -> Form e output
initial validation =
  F <|
    { fields = Group Dict.empty
    , dirtyFields = Set.empty
    , visitedFields = Set.empty
    , isSubmitted = False
    , output = Nothing
    , errors = GroupErrors Dict.empty
    , validation = validation
    }


{-| Form action -}
type Action
  = NoOp
  | UpdateField String Field
  | Validate
  | Submit


updateStringAt : String -> String -> Action
updateStringAt name s =
  UpdateField name (Text s)


updateBoolAt : String -> Bool -> Action
updateBoolAt name b =
  UpdateField name (Check b)


validate : Action
validate =
  Validate


submit : Action
submit =
  Submit


{-| Update for direct usage. -}
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


{-| -}
getFieldAt : String -> Form e o -> Maybe Field
getFieldAt qualifiedName (F model) =
  let
    walkPath name maybeField =
      case maybeField of
        Just field ->
          Field.getAt name field
        Nothing ->
          Nothing
  in
    List.foldl walkPath (Just model.fields) (String.split "." qualifiedName)


{-| -}
getStringAt : String -> Form e o -> Maybe String
getStringAt name form =
  getFieldAt name form `Maybe.andThen` getString


{-| -}
getBoolAt : String -> Form e o -> Maybe Bool
getBoolAt name form =
  getFieldAt name form `Maybe.andThen` getBool


{-| -}
setFieldAt : String -> Field -> Form e o -> Field
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
    walkPath (String.split "." qualifiedName) (Just model.fields)


{-| -}
getErrorAt : String -> Form e o -> Maybe (Error e)
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


getOutput : Form e o -> Maybe o
getOutput (F model) =
  model.output


isSubmitted : Form e o -> Bool
isSubmitted (F model) =
  model.isSubmitted


isVisitedAt : String -> Form e o -> Bool
isVisitedAt qualifiedName (F model) =
  Set.member qualifiedName model.visitedFields


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

