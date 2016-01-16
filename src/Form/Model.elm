module Form.Model where

import Dict exposing (Dict)


type Action
  = NoOp
  | UpdateField String FieldValue
  | Validate


type alias Form =  Dict String Field

getValue : Form -> String -> Maybe FieldValue
getValue form name =
  Dict.get name form
    |> Maybe.map .value

getErrors : Form -> String -> List FieldError
getErrors form name =
  Dict.get name form
    |> Maybe.map .errors
    |> Maybe.withDefault []


type alias Field =
  { value : FieldValue
  , errors : List FieldError
  }

type alias Setup value action =
  { validate : Form -> FormResult value
  , address : Signal.Address action
  , okHandler : value -> action
  , errHandler : action
  }

type alias FormResult a =
  Result FormErrors a

type alias FormErrors =
  Dict String (List FieldError)

type alias ValidationResult a =
  Result FieldError a

type alias FieldValidator v =
  { name : String
  , validate : Maybe FieldValue -> ValidationResult v
  }

type alias FieldValue = String


type FieldError
  = Empty
  | InvalidDate
  | InvalidInt
  | LowerThan Int
  | GreaterThan Int
  | ShorterThan Int
  | LongerThan Int
  | CustomError String

