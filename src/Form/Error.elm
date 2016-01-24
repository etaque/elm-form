module Form.Error (Error(..), getAt) where

import Dict exposing (Dict)


{-| A validation error. See `Validate.customError` for `CustomError` building. -}
type Error e
  = GroupErrors (Dict String (Error e))
  | Empty
  | InvalidString
  | InvalidEmail
  | InvalidInt
  | InvalidFloat
  | InvalidBool
  | InvalidDate
  | SmallerIntThan Int
  | GreaterIntThan Int
  | SmallerFloatThan Float
  | GreaterFloatThan Float
  | ShorterStringThan Int
  | LongerStringThan Int
  | NotIncludedIn
  | CustomError e


getAt : String -> (Error e) -> Maybe (Error e)
getAt name error =
  case error of
    GroupErrors groupErrors ->
      Dict.get name groupErrors
    _ ->
      Nothing
