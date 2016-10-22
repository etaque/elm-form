module Form.Error exposing (Error(..), getAt)

{-| Validation errors.

@docs Error, getAt
-}

import Dict exposing (Dict)


{-| A validation error. See `Form.Validate.customError` for `CustomError` building.
-}
type Error e
    = GroupErrors (Dict String (Error e))
    | Empty
    | InvalidString
    | InvalidEmail
    | InvalidUrl
    | InvalidFormat
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


{-| Get error at name, for nested errors.
-}
getAt : String -> Error e -> Maybe (Error e)
getAt name error =
    case error of
        GroupErrors groupErrors ->
            Dict.get name groupErrors

        _ ->
            Nothing
