module Form.Model where

import Dict exposing (Dict)
import String


-- Actions


type Action
  = NoOp
  | UpdateField String Value
  | Validate


updateString : String -> String -> Action
updateString name s =
  UpdateField name (TextField s)


updateBool : String -> Bool -> Action
updateBool name b =
  UpdateField name (CheckBox b)


-- Model


type alias Form customError target action =
  { value : Value
  , errors : ValidationError customError
  , setup : Setup customError target action
  }


type alias WithForm customError target action model = { model | form : Form customError target action }


type Value
  = Group Fields
  | TextField String
  | CheckBox Bool
  | Multi String
  | EmptyValue


type alias Fields = Dict String Value


type alias Setup customError target action =
  { validation : Validation customError target
  , initialFields : Fields
  , onOk : target -> action
  , onErr : action
  }


type alias Validation customError value =
  Value -> Result (ValidationError customError) value


type ValidationError customError
  = GroupErrors (Dict String (ValidationError customError))
  | ValueError (Error customError)


getValue : Form e target action -> Value
getValue =
  .value


getFieldAt : String -> Form e t a -> Maybe Value
getFieldAt qualifiedName form =
  let
    walkPath name maybeValue =
      case maybeValue of
        Just value ->
          getField name value
        Nothing ->
          Nothing
  in
    List.foldl walkPath (Just form.value) (String.split "." qualifiedName)

getStringAt : String -> Form e t a -> Maybe String
getStringAt name form =
  getFieldAt name form `Maybe.andThen` getString


getField : String -> Value -> Maybe Value
getField name value =
  case value of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing

setFieldAt : String -> Value -> Form e t a -> Value
setFieldAt qualifiedName value form =
  let
    walkPath path maybeNode =
      case path of
        name :: rest ->
          let
            node = Maybe.withDefault (Group Dict.empty) maybeNode
            childValue = walkPath rest (getField name node)
          in
            mergeGroups (Group (Dict.fromList [ (name, childValue) ])) node
        [] ->
          value
  in
    walkPath (String.split "." qualifiedName) (Just form.value)


mergeGroups : Value -> Value -> Value
mergeGroups v1 v2 =
  Group <| case (v1, v2) of
    (Group g1, Group g2) ->
      Dict.union g1 g2
    (Group g1, _) ->
      g1
    (_, Group g2) ->
      g2
    _ ->
      Dict.empty


-- setField : String -> Value -> Value -> Value
-- setField fieldName fieldValue onGroup =
--   case onGroup of
--     Group fields ->
--       Group (Dict.insert fieldName fieldValue fields)
--     _ ->
--       Group (Dict.fromList [ (fieldName, fieldValue) ])


getBool : Value -> Maybe Bool
getBool value =
  case value of
    CheckBox b ->
      Just b
    _ ->
      Nothing


getString : Value -> Maybe String
getString value =
  case value of
    TextField s ->
      Just s
    _ ->
      Nothing


getError : String -> Form e target action -> Maybe (Error e)
getError name form =
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


type Error e
  = EmptyError
  | InvalidString
  | InvalidInt
  | InvalidFloat
  | InvalidBool
  | InvalidDate
  | SmallerThan Int
  | GreaterThan Int
  | ShorterThan Int
  | LongerThan Int
  | CustomError e

