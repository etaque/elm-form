module Form.Field
  ( Field (..), getAt, getString, getBool
  ) where

import Dict exposing (Dict)


{-| Form field. Can either be a group of named fields, or a final field. -}
type Field
  = Group (Dict String Field)
  | Text String
  | Check Bool
  | Multi String


{-| Private -}
getAt : String -> Field -> Maybe Field
getAt name field =
  case field of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing

getBool : Field -> Maybe Bool
getBool field =
  case field of
    Check b ->
      Just b
    _ ->
      Nothing


getString : Field -> Maybe String
getString field =
  case field of
    Text s ->
      Just s
    _ ->
      Nothing

