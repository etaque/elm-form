module Form.Field exposing (Field(..), group, at, asString, asBool)

{-| Read and write field values.

# Constructors
@docs Field, group

# Value readers
@docs at, asString, asBool
-}

import Dict exposing (Dict)


{-| Form field. Can either be a group of named fields, or a final field.
-}
type Field
    = Group (Dict String Field)
    | Text String
    | Textarea String
    | Select String
    | Radio String
    | Check Bool
    | EmptyField


{-| Build a group of values, for nested forms.
-}
group : List ( String, Field ) -> Field
group =
    Group << Dict.fromList


{-| Get field at name, for nested forms.
-}
at : String -> Field -> Maybe Field
at name field =
    case field of
        Group fields ->
            Dict.get name fields

        _ ->
            Nothing


{-| Get field value as boolean.
-}
asBool : Field -> Maybe Bool
asBool field =
    case field of
        Check b ->
            Just b

        _ ->
            Nothing


{-| Get field value as string.
-}
asString : Field -> Maybe String
asString field =
    case field of
        Text s ->
            Just s

        Textarea s ->
            Just s

        Select s ->
            Just s

        Radio s ->
            Just s

        _ ->
            Nothing
