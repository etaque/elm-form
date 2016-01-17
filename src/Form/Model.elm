module Form.Model where

import Dict exposing (Dict)


type Action
  = NoOp
  | UpdateField String FieldValue
  | Validate

updateString : String -> String -> Action
updateString name s =
  UpdateField name (Text s)


updateBool : String -> Bool -> Action
updateBool name b =
  UpdateField name (Check b)


type alias Form =  Dict String Field

getValue : Form -> String -> Maybe FieldValue
getValue form name =
  Dict.get name form
    |> Maybe.map .value

getBool : Form -> String -> Maybe Bool
getBool form name =
  case getValue form name of
    Just (Check b) ->
      Just b
    _ ->
      Nothing

getString : Form -> String -> Maybe String
getString form name =
  case getValue form name of
    Just (Text s) ->
      Just s
    _ ->
      Nothing

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

type FieldValue
  = EmptyValue
  | Text String
  | Check Bool
  | Multi String


type FieldError
  = Empty
  | InvalidString
  | InvalidInt
  | InvalidFloat
  | InvalidBool
  | InvalidDate
  | LowerThan Int
  | GreaterThan Int
  | ShorterThan Int
  | LongerThan Int
  | CustomError (List String)

