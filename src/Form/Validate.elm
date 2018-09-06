module Form.Validate exposing
    ( Validation, field, map, succeed, andThen, andMap, customError, defaultValue, mapError, withCustomError, sequence
    , map2, map3, map4, map5, map6, map7, map8
    , list, string, int, float, bool, maybe, email, emptyString
    , minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn
    , fail, customValidation, oneOf
    )

{-| Form validation.


# Combinators

@docs Validation, field, map, succeed, andThen, andMap, customError, defaultValue, mapError, withCustomError, sequence


# Fixed-size forms

@docs map2, map3, map4, map5, map6, map7, map8


# Type extractors

@docs list, string, int, float, bool, maybe, email, emptyString


# Common filters

@docs minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, format, includedIn


# Custom validations

@docs fail, customValidation, oneOf

-}

import Dict exposing (Dict)
import Form.Error as Error exposing (Error, ErrorValue(..))
import Form.Field as Field exposing (Field, FieldValue(..))
import Form.Tree as Tree
import Regex exposing (Regex)
import Result
import String


{-| A validation is a function that takes a form field and returns a result
being either a validation error or the expected object.
-}
type alias Validation customError output =
    Field -> Result (Error customError) output


{-| Map over the result of the validation.

    field "myfield" (string |> map String.trim)

-}
map : (a -> b) -> Validation e a -> Validation e b
map f validation validationField =
    Result.map f (validation validationField)


{-| Apply a new validation to the result of the validation.

    field "myfield" (int |> andThen (minInt 10))

-}
andThen : (a -> Validation e b) -> Validation e a -> Validation e b
andThen callback validation validationField =
    validation validationField |> Result.andThen (\next -> callback next validationField)


{-| Incremental form validation for records with more that 8 fields.

    Form.Validate.succeed SomeRecord
        |> andMap (field "foo" string)
        |> andMap (field "bar" string)

-}
andMap : Validation e a -> Validation e (a -> b) -> Validation e b
andMap aValidation partialValidation validationField =
    case ( partialValidation validationField, aValidation validationField ) of
        ( Ok partial, Ok a ) ->
            Ok (partial a)

        ( partialResult, aResult ) ->
            Err (mergeMany [ errMaybe partialResult, errMaybe aResult ])


{-| Rescue a failed validation with the supplied value.
-}
defaultValue : a -> Validation e a -> Validation e a
defaultValue a validation validationField =
    Ok (Result.withDefault a (validation validationField))


{-| Call Result.mapError on validation result.
-}
mapError : (Error e1 -> Error e2) -> Validation e1 a -> Validation e2 a
mapError f validation =
    \validationField -> Result.mapError f (validation validationField)


{-| Arrange that if a validation fails, it has the given custom error.

    field "customerId"
        (V.int
            |> andThen (minInt 1)
            |> andThen (maxInt 9999)
            |> withCustomError InvalidIdentity
        )

-}
withCustomError : customErr -> Validation e a -> Validation customErr a
withCustomError =
    mapError << always << customError


{-| Helper to create a CustomError.
-}
customError : e -> Error e
customError =
    CustomError >> Error.value


{-| Access the given field in the group.

    field "name" string

-}
field : String -> Validation e a -> Validation e a
field key validation validationField =
    Tree.getAtName key validationField
        |> Maybe.withDefault (Tree.Value EmptyField)
        |> validation
        |> Result.mapError
            (\e -> Tree.group [ ( key, e ) ])


{-| Validation a form with two fields.
-}
map2 : (a -> b -> m) -> Validation e a -> Validation e b -> Validation e m
map2 func v1 v2 =
    map func v1
        |> andMap v2



-- apply (form1 func v1) v2


{-| Validation a form with three fields.
-}
map3 : (a -> b -> c -> m) -> Validation e a -> Validation e b -> Validation e c -> Validation e m
map3 func v1 v2 v3 =
    map2 func v1 v2
        |> andMap v3


{-| Validation a form with four fields.
-}
map4 : (a -> b -> c -> d -> m) -> Validation e a -> Validation e b -> Validation e c -> Validation e d -> Validation e m
map4 func v1 v2 v3 v4 =
    map3 func v1 v2 v3
        |> andMap v4


{-| Validation a form with five fields.
-}
map5 : (a -> b -> c -> d -> e -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err m
map5 func v1 v2 v3 v4 v5 =
    map4 func v1 v2 v3 v4
        |> andMap v5


{-| Validation a form with six fields.
-}
map6 : (a -> b -> c -> d -> e -> f -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err m
map6 func v1 v2 v3 v4 v5 v6 =
    map5 func v1 v2 v3 v4 v5
        |> andMap v6


{-| Validation a form with seven fields.
-}
map7 : (a -> b -> c -> d -> e -> f -> g -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err g -> Validation err m
map7 func v1 v2 v3 v4 v5 v6 v7 =
    map6 func v1 v2 v3 v4 v5 v6
        |> andMap v7


{-| Validation a form with eight fields.
-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> m) -> Validation err a -> Validation err b -> Validation err c -> Validation err d -> Validation err e -> Validation err f -> Validation err g -> Validation err h -> Validation err m
map8 func v1 v2 v3 v4 v5 v6 v7 v8 =
    map7 func v1 v2 v3 v4 v5 v6 v7
        |> andMap v8


{-| Private
-}
mergeMany : List (Maybe (Error e)) -> Error e
mergeMany errors =
    errors
        |> List.filterMap identity
        |> List.foldl groupErrorsUnion (Tree.group [])


{-| Private
-}
groupErrorsUnion : Error e -> Error e -> Error e
groupErrorsUnion e1 e2 =
    case ( e1, e2 ) of
        ( Tree.Group g1, Tree.Group g2 ) ->
            Tree.Group (Dict.union g1 g2)

        _ ->
            e2


{-| Private
-}
errMaybe : Result e a -> Maybe e
errMaybe res =
    case res of
        Ok _ ->
            Nothing

        Err e ->
            Just e


{-| Validation an integer using `String.toInt`.
-}
int : Validation e Int
int v =
    Field.asString v
        |> Maybe.andThen String.toInt
        |> Result.fromMaybe (Error.value InvalidInt)


{-| Validation a float using `String.toFloat`.
-}
float : Validation e Float
float v =
    Field.asString v
        |> Maybe.andThen String.toFloat
        |> Result.fromMaybe (Error.value InvalidFloat)


{-| Validation a String.
-}
string : Validation e String
string v =
    case Field.asString v of
        Just s ->
            if String.isEmpty s then
                Err (Error.value Empty)

            else
                Ok s

        Nothing ->
            Err (Error.value InvalidString)


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
                Err (Error.value InvalidString)

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


{-| Transform validation result to `Maybe`, using `Result.toMaybe`.
-}
maybe : Validation e a -> Validation e (Maybe a)
maybe validation validationField =
    Ok (Result.toMaybe (validation validationField))


{-| Fails if `String.isEmpty`.
-}
nonEmpty : String -> Validation e String
nonEmpty s validationField =
    if String.isEmpty s then
        Err (Error.value Empty)

    else
        Ok s


{-| Min length for String.
-}
minLength : Int -> String -> Validation e String
minLength min s validationField =
    if String.length s >= min then
        Ok s

    else
        Err (Error.value (ShorterStringThan min))


{-| Max length for String.
-}
maxLength : Int -> String -> Validation e String
maxLength max s validationField =
    if String.length s <= max then
        Ok s

    else
        Err (Error.value (LongerStringThan max))


{-| Min value for Int.
-}
minInt : Int -> Int -> Validation e Int
minInt min i validationField =
    if i >= min then
        Ok i

    else
        Err (Error.value (SmallerIntThan min))


{-| Max value for Int.
-}
maxInt : Int -> Int -> Validation e Int
maxInt max i validationField =
    if i <= max then
        Ok i

    else
        Err (Error.value (GreaterIntThan max))


{-| Min value for Float.
-}
minFloat : Float -> Float -> Validation e Float
minFloat min i validationField =
    if i >= min then
        Ok i

    else
        Err (Error.value (SmallerFloatThan min))


{-| Max value for Float.
-}
maxFloat : Float -> Float -> Validation e Float
maxFloat max i validationField =
    if i <= max then
        Ok i

    else
        Err (Error.value (GreaterFloatThan max))


{-| Validates format of the string.
-}
format : Regex -> String -> Validation e String
format regex s validationField =
    if Regex.contains regex s then
        Ok s

    else
        Err (Error.value InvalidFormat)


{-| Stolen to elm-validate.
-}
validEmailPattern : Regex
validEmailPattern =
    "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
        |> Regex.fromStringWith { caseInsensitive = True, multiline = False }
        |> Maybe.withDefault Regex.never


{-| Check if the string is a valid email address.
-}
email : Validation e String
email =
    string
        |> andThen
            (\s ->
                format validEmailPattern s
                    |> mapError (\_ -> Error.value InvalidEmail)
            )


{-| Check if the string is included in the given list.
-}
includedIn : List String -> String -> Validation e String
includedIn items s validationField =
    if List.member s items then
        Ok s

    else
        Err (Error.value NotIncludedIn)


{-| A validation that always fails. Useful for contextual validation.
-}
fail : Error e -> Validation e a
fail error validationField =
    Err error


{-| A validation that always succeeds. Useful for contextual validation.
-}
succeed : a -> Validation e a
succeed a validationField =
    Ok a


{-| Custom validation for your special cases.
-}
customValidation : Validation e a -> (a -> Result (Error e) b) -> Validation e b
customValidation validation callback validationField =
    validation validationField |> Result.andThen callback


{-| First successful validation wins, from left to right.
-}
oneOf : List (Validation e a) -> Validation e a
oneOf validations validationField =
    let
        results =
            List.map (\v -> v validationField) validations

        walkResults result combined =
            case ( combined, result ) of
                ( Ok _, _ ) ->
                    combined

                _ ->
                    result
    in
    List.foldl walkResults (Err (Error.value Empty)) results


{-| Combine a list of validations into a validation producing a list of all
results.
-}
sequence : List (Validation e a) -> Validation e (List a)
sequence validations =
    List.foldr (map2 (::)) (succeed []) validations


{-| Validate a list of fields.
-}
list : Validation e a -> Validation e (List a)
list validation validationField =
    case validationField of
        Tree.List items ->
            let
                results =
                    List.map validation items

                indexedErrMaybe index res =
                    case res of
                        Ok _ ->
                            Nothing

                        Err e ->
                            Just ( String.fromInt index, e )

                errors =
                    results
                        |> List.indexedMap indexedErrMaybe
                        |> List.filterMap identity
            in
            if List.isEmpty errors then
                Ok (List.filterMap Result.toMaybe results)

            else
                Err (Tree.group errors)

        _ ->
            Ok []
