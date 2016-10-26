module Form.Field exposing (Field, FieldValue(..), group, list, value, asString, asBool)

{-| Read and write field values.

# Constructors
@docs Field, FieldValue, group, list, value


# Value readers
@docs  asString, asBool
-}

import Form.Tree as Tree exposing (Tree)


{-| A field is a tree node.
-}
type alias Field =
    Tree FieldValue


{-| Form field. Can either be a group of named fields, or a final field.
-}
type FieldValue
    = Text String
    | Textarea String
    | Select String
    | Radio String
    | Check Bool
    | EmptyField


{-| Build a group of values, for nested forms.
-}
group : List ( String, Field ) -> Field
group =
    Tree.group


{-| Build a list of values, for dynamic fields list
-}
list : List Field -> Field
list =
    Tree.list


{-| Build a field from its value.
-}
value : FieldValue -> Field
value =
    Tree.Value


{-| Get field value as boolean.
-}
asBool : Field -> Maybe Bool
asBool node =
    case node of
        Tree.Value (Check b) ->
            Just b

        _ ->
            Nothing


{-| Get field value as string.
-}
asString : Field -> Maybe String
asString field =
    case field of
        Tree.Value value ->
            case value of
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

        _ ->
            Nothing
