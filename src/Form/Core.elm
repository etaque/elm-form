module Form.Core where

import Dict exposing (Dict)

import Form.Error exposing (Error)
import Form.Value exposing (Value)


type alias Setup customError target action =
  { validation : Validation customError target
  , initialFields : Dict String Value
  , onOk : target -> action
  , onErr : action
  }


type alias Validation customError value =
  Value -> Result (ValidationError customError) value


type ValidationError customError
  = GroupErrors (Dict String (ValidationError customError))
  | ValueError (Error customError)


