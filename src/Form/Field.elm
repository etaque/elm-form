module Form.Field
  ( Field (..), at, asString, asBool, text, select, radio, check, group
  ) where

{-| Read and write field values.

@docs Field (..)

# Read field value
@docs at, asString, asBool

# Write field value, for init.
@docs text, select, radio, check, group
-}

import Dict exposing (Dict)


{-| Form field. Can either be a group of named fields, or a final field. -}
type Field
  = Group (Dict String Field)
  | Text String
  | Check Bool
  | EmptyField


{-| Build a text field value. -}
text : String -> Field
text =
  Text


{-| Build a text field value. -}
select : String -> Field
select =
  text


{-| Build a radio button value. -}
radio : String -> Field
radio =
  text


{-| Build a checkbox value. -}
check : Bool -> Field
check =
  Check


{-| Build a group of values, for nested forms. -}
group : List (String, Field) -> Field
group =
  Group << Dict.fromList


{-| Get field at name, for nested forms. -}
at : String -> Field -> Maybe Field
at name field =
  case field of
    Group fields ->
      Dict.get name fields
    _ ->
      Nothing


{-| Get field value as boolean. -}
asBool : Field -> Maybe Bool
asBool field =
  case field of
    Check b ->
      Just b
    _ ->
      Nothing


{-| Get field value as string. -}
asString : Field -> Maybe String
asString field =
  case field of
    Text s ->
      Just s
    _ ->
      Nothing

