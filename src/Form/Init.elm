module Form.Init exposing (setBool, setString, setGroup, setList)

{-| Helpers for initial fields values

@docs setBool, setString, setGroup, setList

-}

import Form.Field as Field exposing (Field, FieldValue)


{-| Builds a tuple of field name and boolean value
-}
setBool : String -> Bool -> ( String, Field )
setBool name b =
    ( name, Field.bool b )


{-| Builds a tuple of field name and string value
-}
setString : String -> String -> ( String, Field )
setString name str =
    ( name, Field.string str )


{-| Build a group of values, for nested forms.
-}
setGroup : String -> List ( String, Field ) -> ( String, Field )
setGroup name pairs =
    ( name, Field.group pairs )


{-| Build a list of values, for dynamic fields setList
-}
setList : String -> List Field -> ( String, Field )
setList name items =
    ( name, Field.list items )
