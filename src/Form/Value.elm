module Form.Value
  ( Value (..), getAt, getString, getBool
  ) where

import Dict exposing (Dict)


{-| Form value. Can either be a group of named values, or a final value. -}
type Value
  = Group (Dict String Value)
  | Text String
  | Check Bool
  | Multi String


{-| Private -}
getAt : String -> Value -> Maybe Value
getAt name value =
  case value of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing

getBool : Value -> Maybe Bool
getBool value =
  case value of
    Check b ->
      Just b
    _ ->
      Nothing


getString : Value -> Maybe String
getString value =
  case value of
    Text s ->
      Just s
    _ ->
      Nothing

