module Form.Field exposing (Field, FieldValue(..), field, group, list, value, asString, asBool)

{-| Read and write field values.

# Constructors
@docs Field, FieldValue, field, group, list, value


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


{-| Builds a tuple of field name and value, for groups.
-}
field : String -> FieldValue -> ( String, Field )
field name value =
    ( name, Tree.Value value )


{-| Build a group of values, for nested forms.
-}
group : String -> List ( String, Field ) -> ( String, Field )
group name pairs =
    ( name, Tree.group pairs )


{-| Build a list of values, for dynamic fields list
-}
list : String -> List Field -> ( String, Field )
list name items =
    ( name, Tree.list items )


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
