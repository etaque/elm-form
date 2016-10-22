module Form.Validate exposing (Validation, get, map, andThen, pipeTo, apply, customError, defaultValue, formatError, withCustomError, form1, form2, form3, form4, form5, form6, form7, form8, string, int, float, bool, date, maybe, email, url, emptyString, minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn, fail, succeed, customValidation, oneOf)

{-| Form validation.

# Combinators
@docs Validation, get, map, succeed, andThen, pipeTo, apply, customError, defaultValue, formatError, withCustomError

# Fixed-size forms
@docs form1, form2, form3, form4, form5, form6, form7, form8

# Type extractors
@docs string, int, float, bool, date, maybe, email, url, emptyString

# Common filters
@docs minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn

# Custom validations
@docs fail, succeed, customValidation, oneOf
-}

import Result
import Date exposing (Date)
import Dict exposing (Dict)
import String
import Regex exposing (Regex)
import Form.Error as Error exposing (Error(..))
import Form.Field as Field exposing (Field(..))


{-| A validation is a function that takes a form field and returns a result
being either a validation error or the expected object.
-}
type alias Validation customError output =
    Field -> Result (Error customError) output


{-| Map over the result of the validation.

    get "myfield" (string |> map String.trim)
-}
map : (a -> b) -> Validation e a -> Validation e b
map f validation field =
    Result.map f (validation field)


{-| Apply a new validation to the result of the validation.

    get "myfield" (int `andThen` minInt 10)
-}
andThen : Validation e a -> (a -> Validation e b) -> Validation e b
andThen validation callback field =
    validation field `Result.andThen` (\next -> (callback next) field)


{-| Same as `andThen`, but flipped for piping.

    int |> pipeTo (minInt 10)
-}
pipeTo : (a -> Validation e b) -> Validation e a -> Validation e b
pipeTo =
    flip andThen


{-| Incremental form validation for records with more that 8 fields.

    Form.succeed SomeRecord
      `apply` (get "foo" string)
      `apply` (get "bar" string)
-}
apply : Validation e (a -> b) -> Validation e a -> Validation e b
apply partialValidation aValidation field =
    case ( partialValidation field, aValidation field ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (mergeMany [ getErr partialResult, getErr aResult ])


{-| Rescue a failed validation with the supplied value.
-}
defaultValue : a -> Validation e a -> Validation e a
defaultValue a validation field =
    Ok (Result.withDefault a (validation field))


{-| Call Result.formatError on validation result.
-}
formatError : (Error e1 -> Error e2) -> Validation e1 a -> Validation e2 a
formatError f validation =
    \field -> Result.formatError f (validation field)


{-| Arrange that if a validation fails, it has the given custom error.

    get "customerId" (V.int
          `andThen` minInt 1
          `andThen` maxInt 9999
          |> withCustomError InvalidIdentity)
-}
withCustomError : customErr -> Validation e a -> Validation customErr a
withCustomError =
    formatError << always << customError


{-| Helper to create a CustomError.
-}
customError : e -> Error e
customError =
    CustomError


{-| Access the given field in the group.

    get "name" string
-}
get : String -> Validation e a -> Validation e a
get key validation field =
    Field.at key field
        |> Maybe.withDefault EmptyField
        |> validation
        |> Result.formatError
            (\e -> GroupErrors (Dict.fromList [ ( key, e ) ]))


{-| Validation a form with one field.
-}
form1 : (a -> field) -> Validation e a -> Validation e field
form1 =
    map


{-| Validation a form with two fields.
-}
form2 : (a -> b -> m) -> Validation e a -> Validation e b -> Validation e m
form2 func v1 v2 =
    (form1 func v1) `apply` v2


{-| Validation a form with three fields.
-}
form3 : (a -> b -> c -> m) -> Validation e a -> Validation e b -> Validation e c -> Validation e m
form3 func v1 v2 v3 =
    (form2 func v1 v2) `apply` v3


{-| Validation a form with four fields.
-}
form4 : (a -> b -> c -> d -> m) -> Validation e a -> Validation e b -> Validation e c -> Validation e d -> Validation e m
form4 func v1 v2 v3 v4 =
    (form3 func v1 v2 v3) `apply` v4


{-| Validation a form with five fields.
-}
form5 : (a -> b -> c -> d -> e -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err m
form5 func v1 v2 v3 v4 v5 =
    (form4 func v1 v2 v3 v4) `apply` v5


{-| Validation a form with six fields.
-}
form6 : (a -> b -> c -> d -> e -> f -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err m
form6 func v1 v2 v3 v4 v5 v6 =
    (form5 func v1 v2 v3 v4 v5) `apply` v6


{-| Validation a form with seven fields.
-}
form7 : (a -> b -> c -> d -> e -> f -> g -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err g -> Validation err m
form7 func v1 v2 v3 v4 v5 v6 v7 =
    (form6 func v1 v2 v3 v4 v5 v6) `apply` v7


{-| Validation a form with eight fields.
-}
form8 : (a -> b -> c -> d -> e -> f -> g -> h -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err g -> Validation err h -> Validation err m
form8 func v1 v2 v3 v4 v5 v6 v7 v8 =
    (form7 func v1 v2 v3 v4 v5 v6 v7) `apply` v8


{-| Private
-}
mergeMany : List (Maybe (Error e)) -> Error e
mergeMany errors =
    errors
        |> List.filterMap identity
        |> List.foldl groupErrorsUnion (GroupErrors Dict.empty)


{-| Private
-}
groupErrorsUnion : Error e -> Error e -> Error e
groupErrorsUnion e1 e2 =
    case ( e1, e2 ) of
        ( GroupErrors ge1, GroupErrors ge2 ) ->
            GroupErrors (Dict.union ge1 ge2)

        _ ->
            e2


{-| Private
-}
getErr : Result e a -> Maybe e
getErr res =
    case res of
        Ok _ ->
            Nothing

        Err e ->
            Just e


{-| Validation an integer using `String.toInt`.
-}
int : Validation e Int
int v =
    case Field.asString v of
        Just s ->
            String.toInt s
                |> Result.formatError (\_ -> InvalidInt)

        Nothing ->
            Err InvalidInt


{-| Validation a float using `String.toFloat`.
-}
float : Validation e Float
float v =
    case Field.asString v of
        Just s ->
            String.toFloat s
                |> Result.formatError (\_ -> InvalidFloat)

        Nothing ->
            Err InvalidFloat


{-| Validation a String.
-}
string : Validation e String
string v =
    case Field.asString v of
        Just s ->
            if String.isEmpty s then
                Err Empty
            else
                Ok s

        Nothing ->
            Err InvalidString


{-| Validate an empty string, otherwise failing with InvalidString.
Useful with `oneOf` for optional fields with format validation.
-}
emptyString : Validation e String
emptyString v =
    case Field.asString v of
        Just s ->
            if String.isEmpty s then
                Ok s
            else
                Err InvalidString

        Nothing ->
            Ok ""


{-| Validation a Bool.
-}
bool : Validation e Bool
bool v =
    case Field.asBool v of
        Just b ->
            Ok b

        Nothing ->
            Ok False


{-| Validation a Date using `Date.fromString`.
-}
date : Validation e Date
date v =
    case Field.asString v of
        Just s ->
            Date.fromString s
                |> Result.formatError (\_ -> InvalidDate)

        Nothing ->
            Err InvalidDate


{-| Transform validation result to `Maybe`, using `Result.toMaybe`.
-}
maybe : Validation e a -> Validation e (Maybe a)
maybe validation field =
    Ok (Result.toMaybe (validation field))


{-| Fails if `String.isEmpty`.
-}
nonEmpty : String -> Validation e String
nonEmpty s field =
    if String.isEmpty s then
        Err Empty
    else
        Ok s


{-| Min length for String.
-}
minLength : Int -> String -> Validation e String
minLength min s field =
    if String.length s >= min then
        Ok s
    else
        Err (ShorterStringThan min)


{-| Max length for String.
-}
maxLength : Int -> String -> Validation e String
maxLength max s field =
    if String.length s <= max then
        Ok s
    else
        Err (LongerStringThan max)


{-| Min value for Int.
-}
minInt : Int -> Int -> Validation e Int
minInt min i field =
    if i >= min then
        Ok i
    else
        Err (SmallerIntThan min)


{-| Max value for Int.
-}
maxInt : Int -> Int -> Validation e Int
maxInt max i field =
    if i <= max then
        Ok i
    else
        Err (GreaterIntThan max)


{-| Min value for Float.
-}
minFloat : Float -> Float -> Validation e Float
minFloat min i field =
    if i >= min then
        Ok i
    else
        Err (SmallerFloatThan min)


{-| Max value for Float.
-}
maxFloat : Float -> Float -> Validation e Float
maxFloat max i field =
    if i <= max then
        Ok i
    else
        Err (GreaterFloatThan max)


{-| Validates format of the string.
-}
format : Regex -> String -> Validation e String
format regex s field =
    if Regex.contains regex s then
        Ok s
    else
        Err InvalidFormat


{-| Stolen to elm-validate.
-}
validEmailPattern : Regex
validEmailPattern =
    Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.caseInsensitive


{-| Check if the string is a valid email address.
-}
email : Validation e String
email =
    string
        `andThen`
            (\s ->
                format validEmailPattern s
                    |> formatError (\_ -> InvalidEmail)
            )


validUrlPattern : Regex
validUrlPattern =
    Regex.regex "^(https?://)?([\\da-z\\.-]+)\\.([a-z\\.]{2,6})([\\w \\.-]*)*/?$"
        |> Regex.caseInsensitive


{-| Check if the string is a valid URL.
-}
url : Validation e String
url =
    string
        `andThen`
            (\s ->
                format validUrlPattern s
                    |> formatError (\_ -> InvalidUrl)
            )


{-| Check if the string is included in the given list.
-}
includedIn : List String -> String -> Validation e String
includedIn items s field =
    if List.member s items then
        Ok s
    else
        Err NotIncludedIn


{-| A validation that always fails. Useful for contextual validation.
-}
fail : Error e -> Validation e a
fail error field =
    Err error


{-| A validation that always succeeds. Useful for contextual validation.
-}
succeed : a -> Validation e a
succeed a field =
    Ok a


{-| Custom validation for your special cases.
-}
customValidation : Validation e a -> (a -> Result (Error e) b) -> Validation e b
customValidation validation callback field =
    validation field `Result.andThen` callback


{-| First successful validation wins, from left to right.
-}
oneOf : List (Validation e a) -> Validation e a
oneOf validations field =
    let
        results =
            List.map (\v -> v field) validations

        walkResults result combined =
            case ( combined, result ) of
                ( Ok _, _ ) ->
                    combined

                _ ->
                    result
    in
        List.foldl walkResults (Err Empty) results
