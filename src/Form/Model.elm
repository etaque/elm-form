module Form.Model where

import Dict exposing (Dict)


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


type alias Form target action =
  { value : Value
  , errors : ValidationError
  , setup : Setup target action
  }


type alias WithForm target action model = { model | form : Form target action }


type Value
  = Group Fields
  | TextField String
  | CheckBox Bool
  | Multi String
  | EmptyValue


type alias Fields = Dict String Value


type alias Setup target action =
  { validation : Validation target
  , initialFields : Fields
  , onOk : target -> action
  , onErr : action
  }


type alias Validation value =
  Value -> Result ValidationError value


type ValidationError
  = GroupErrors (Dict String ValidationError)
  | ValueError Error


getValue : Form target action -> Value
getValue =
  .value


getField : String -> Value -> Maybe Value
getField name value =
  case value of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing


setField : String -> Value -> Value -> Value
setField fieldName fieldValue value =
  case value of
    Group fields ->
      Group <| Dict.insert fieldName fieldValue fields
    _ ->
      value


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


getErrors : String -> Form target action -> List Error
getErrors name form =
  case form.errors of
    GroupErrors groupErrors ->
      case Dict.get name groupErrors of
        Just ve ->
          case ve of
            ValueError e -> [ e ]
            GroupErrors _ -> []
        Nothing ->
          []
    _ ->
      []


type Error
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
  | CustomError (List String)

