module Form.Field
  ( Field (..), at, asString, asBool, text, select, radio, check, group
  ) where

import Dict exposing (Dict)


{-| Form field. Can either be a group of named fields, or a final field. -}
type Field
  = Group (Dict String Field)
  | Text String
  | Check Bool



text : String -> Field
text =
  Text


select : String -> Field
select =
  text


radio : String -> Field
radio =
  text


check : Bool -> Field
check =
  Check


group : List (String, Field) -> Field
group =
  Group << Dict.fromList


{-| Private -}
at : String -> Field -> Maybe Field
at name field =
  case field of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing

asBool : Field -> Maybe Bool
asBool field =
  case field of
    Check b ->
      Just b
    _ ->
      Nothing


asString : Field -> Maybe String
asString field =
  case field of
    Text s ->
      Just s
    _ ->
      Nothing

