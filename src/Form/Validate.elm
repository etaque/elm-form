module Form.Validate
  ( Validate, get, map, succeed, andThen, pipeTo, apply, customError, defaultValue
  , (:=), (?=), (|:)
  , form1, form2, form3, form4, form5, form6, form7, form8
  , string, int, float, bool, date, maybe
  , minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, email, format, includedIn
  ) where

{-| Form validation.

# Combinators
@docs Validate, get, map, succeed, andThen, pipeTo, apply, customError, defaultValue

# Infix operators
@docs (:=), (?=), (|:)

# Fixed-size forms
@docs form1, form2, form3, form4, form5, form6, form7, form8

# Type extractors
@docs string, int, float, bool, date, maybe

# Filters
@docs minInt, maxInt, minFloat, maxFloat, minLength, maxLength, nonEmpty, email, format, includedIn
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
type alias Validate customError output =
  Field -> Result (Error customError) output


{-| Map over the result of the validation.

    string `map` String.trim
-}
map : (a -> b) -> Validate e a -> Validate e b
map f validation =
  \field -> Result.map f (validation field)


{-| Apply a new validation to the result of the validation.

    int `andThen` (minInt 10)
-}
andThen : Validate e a -> (a -> Validate e b) -> Validate e b
andThen validation callback =
  \field -> validation field `Result.andThen` (\next -> (callback next) field)


{-| Same as `andThen`, but flipped for piping.

    int |> pipeTo (minInt 10)
-}
pipeTo : (a -> Validate e b) -> Validate e a -> Validate e b
pipeTo =
  flip andThen


{-| A validation that never fails.
-}
succeed : a -> Validate e a
succeed a field =
  Ok a


{-| Incremental form validation for records with more that 8 fields.

    Form.succeed SomeRecord
      `apply` ("foo" := string)
      `apply` ("bar" := string)
-}
apply : Validate e (a -> b) -> Validate e a -> Validate e b
apply partialValidation aValidation field =
  case (partialValidation field, aValidation field) of
    (Ok partial, Ok a) ->
      Ok (partial a)
    (partialResult, aResult) ->
      Err (mergeMany [ getErr partialResult, getErr aResult ])


{-| Infix version of `apply`:

    Form.succeed SomeRecord
      |: ("foo" := string)
      |: ("bar" := string)

-}
(|:) : Validate e (a -> b) -> Validate e a -> Validate e b
(|:) = apply


{-| Rescue a failed validation with the supplied value. -}
defaultValue : a -> Validate e a -> Validate e a
defaultValue a validation field =
  Ok (Result.withDefault a (validation field))


{-| Call Result.formatError on validation result. -}
formatError : (Error e -> Error e) -> Validate e a -> Validate e a
formatError f validation =
  \field -> Result.formatError f (validation field)


{-| Transform validation error to the provided custom error. -}
customError : e -> Validate e a -> Validate e a
customError e =
  formatError (\_ -> CustomError e)


{-| private -}
groupError : String -> Error e -> Error e
groupError name e =
  GroupErrors <| Dict.fromList [ (name, e) ]


{-| private -}
err : Error e -> Result (Error e) a
err e =
  Err e


{-| private -}
ifErr : Error e -> Result e' a -> Result (Error e) a
ifErr e res =
  Result.formatError (\_ -> e) res


{-| Access the given field in the group.

    get "name" string
-}
get : String -> Validate e a -> Validate e a
get key validation =
  let
    func v = case v of
      Group fields ->
        case Dict.get key fields of
          Just a -> validation a |> Result.formatError (\e -> groupError key e)
          Nothing -> Err (groupError key Empty)
      _ ->
        Err (groupError key Empty)
  in
    func


{-| Infix version of `get`.

    "name" := string
-}
(:=) : String -> Validate e a -> Validate e a
(:=) =
  get


{-| Access given field, wrapped in a `maybe` (Nothing if error).

    "hobby" ?= string
-}
(?=) : String -> Validate e a -> Validate e (Maybe a)
(?=) s v =
  maybe (get s v)


{-| Validate a form with one field. -}
form1 : (a -> field) -> Validate e a -> Validate e field
form1 =
  map


{-| Validate a form with two fields. -}
form2 : (a -> b -> m) -> Validate e a -> Validate e b -> Validate e m
form2 func v1 v2 =
  (form1 func v1) `apply` v2


{-| Validate a form with three fields. -}
form3 : (a -> b -> c -> m) -> Validate e a -> Validate e b -> Validate e c -> Validate e m
form3 func v1 v2 v3 =
  (form2 func v1 v2) `apply` v3


{-| Validate a form with four fields. -}
form4 : (a -> b -> c -> d -> m) -> Validate e a -> Validate e b -> Validate e c -> Validate e d -> Validate e m
form4 func v1 v2 v3 v4 =
  (form3 func v1 v2 v3) `apply` v4


{-| Validate a form with five fields. -}
form5 : (a -> b -> c -> d -> e -> m) -> Validate err a -> Validate err b -> Validate err c -> Validate err d -> Validate err e -> Validate err m
form5 func v1 v2 v3 v4 v5 =
  (form4 func v1 v2 v3 v4) `apply` v5


{-| Validate a form with six fields. -}
form6 : (a -> b -> c -> d -> e -> f -> m) -> Validate err a -> Validate err b -> Validate err c -> Validate err d -> Validate err e -> Validate err f -> Validate err m
form6 func v1 v2 v3 v4 v5 v6 =
  (form5 func v1 v2 v3 v4 v5) `apply` v6


{-| Validate a form with seven fields. -}
form7 : (a -> b -> c -> d -> e -> f -> g -> m) -> Validate err a -> Validate err b -> Validate err c -> Validate err d -> Validate err e -> Validate err f -> Validate err g -> Validate err m
form7 func v1 v2 v3 v4 v5 v6 v7 =
  (form6 func v1 v2 v3 v4 v5 v6) `apply` v7


{-| Validate a form with eight fields. -}
form8 : (a -> b -> c -> d -> e -> f -> g -> h -> m) -> Validate err a -> Validate err b -> Validate err c -> Validate err d -> Validate err e -> Validate err f -> Validate err g -> Validate err h -> Validate err m
form8 func v1 v2 v3 v4 v5 v6 v7 v8 =
  (form7 func v1 v2 v3 v4 v5 v6 v7) `apply` v8


{-| Private -}
mergeMany : List (Maybe (Error e)) -> Error e
mergeMany errors =
  errors
    |> List.filterMap identity
    |> List.foldl groupErrorsUnion (GroupErrors Dict.empty)


{-| Private -}
groupErrorsUnion : Error e -> Error e -> Error e
groupErrorsUnion e1 e2 =
  case (e1, e2) of
    (GroupErrors ge1, GroupErrors ge2) ->
      GroupErrors (Dict.union ge1 ge2)
    _ ->
      e2


{-| Private -}
getErr : Result e a -> Maybe e
getErr res =
  case res of
    Ok _ -> Nothing
    Err e -> Just e


{-| Validate an integer using `String.toInt`. -}
int : Validate e Int
int v =
  case v of
    Text s ->
      String.toInt s |> ifErr InvalidInt
    _ ->
      err InvalidInt


{-| Validate a float using `String.toFloat`. -}
float : Validate e Float
float v =
  case v of
    Text s ->
      String.toFloat s |> ifErr InvalidFloat
    _ ->
      err InvalidInt


{-| Validate a String. -}
string : Validate e String
string v =
  case v of
    Text s ->
      if String.isEmpty s then
        Err Empty
      else
        Ok s
    _ ->
      err InvalidString


{-| Validate a Bool. -}
bool : Validate e Bool
bool v =
  case v of
    Check b ->
      Ok b
    _ ->
      Ok False


{-| Validate a Date using `Date.fromString`. -}
date : Validate e Date
date v =
  case v of
    Text s ->
      Date.fromString s |> ifErr InvalidDate
    _ ->
      err InvalidDate


{-| Transform validation result to `Maybe`, using `Result.toMaybe`. -}
maybe : Validate e a -> Validate e (Maybe a)
maybe validation field =
  Ok (Result.toMaybe (validation field))


{-| Fails if `String.isEmpty`. -}
nonEmpty : String -> Validate e String
nonEmpty s field =
  if String.isEmpty s then
    err Empty
  else
    Ok s


{-| Min length for String. -}
minLength : Int -> String -> Validate e String
minLength min s field =
  if String.length s >= min then
    Ok s
  else
    err (ShorterStringThan min)


{-| Max length for String. -}
maxLength : Int -> String -> Validate e String
maxLength max s field =
  if String.length s <= max then
    Ok s
  else
    err (ShorterStringThan max)


{-| Min value for Int. -}
minInt : Int -> Int -> Validate e Int
minInt min i field =
  if i >= min then Ok i else err (SmallerIntThan min)


{-| Max value for Int. -}
maxInt : Int -> Int -> Validate e Int
maxInt max i field =
  if i <= max then Ok i else err (GreaterIntThan max)


{-| Min value for Float. -}
minFloat : Float -> Float -> Validate e Float
minFloat min i field =
  if i >= min then Ok i else err (SmallerFloatThan min)


{-| Max value for Float. -}
maxFloat : Float -> Float -> Validate e Float
maxFloat max i field =
  if i <= max then Ok i else err (GreaterFloatThan max)


{-| Validates format of the string. -}
format : String -> Regex -> Validate e String
format s regex field =
  if Regex.contains regex s then
    Ok s
  else
    Err InvalidFormat


{-| Stolen to elm-validate. -}
validEmailPattern : Regex
validEmailPattern =
  Regex.regex "^[a-zA-Z0-9.!#$%&'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$"
    |> Regex.caseInsensitive


{-| Check if the string is a valid email address. -}
email : String -> Validate e String
email s =
  format s validEmailPattern
    |> formatError (\_ -> InvalidEmail)


{-| Check if the string is included in the given list. -}
includedIn : List String -> String -> Validate e String
includedIn items s field =
  if List.member s items then
    Ok s
  else
    Err NotIncludedIn
