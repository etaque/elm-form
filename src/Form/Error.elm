module Form.Error exposing (Error, ErrorValue(..), value)

{-| Validation errors.

@docs Error, ErrorValue, value

-}

import Form.Tree as Tree exposing (Tree)


{-| Tree of errors.
-}
type alias Error e =
    Tree (ErrorValue e)


{-| A validation error. See `Form.Validate.customError` for `CustomError` building.
-}
type ErrorValue e
    = Empty
    | InvalidString
    | InvalidEmail
    | InvalidFormat
    | InvalidInt
    | InvalidFloat
    | InvalidBool
    | SmallerIntThan Int
    | GreaterIntThan Int
    | SmallerFloatThan Float
    | GreaterFloatThan Float
    | ShorterStringThan Int
    | LongerStringThan Int
    | NotIncludedIn
    | CustomError e


{-| Build a tree node (a leaf) for this error
-}
value : ErrorValue a -> Error a
value =
    Tree.Value
