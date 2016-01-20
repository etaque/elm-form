module Form.Validate
  ( Error, Validation, get, map, andThen, customError
  , (:=), (?=), form1, form2, form3
  , string, int, float, bool, date, maybe
  , minInt, maxInt, trim, nonEmpty
  ) where

import Result
import Date exposing (Date)
-- import Time exposing (Time)
import Dict exposing (Dict)
import String

import Form.Model exposing (..)


type alias Error = Form.Model.Error
type alias Validation a = Form.Model.Validation a -- Value -> Result Error a


{-| string |> map lowerCase -}
map : (a -> b) -> Validation a -> Validation b
map f validation =
  \value -> Result.map f (validation value)


{-| int `andThen` (min 5) -}
andThen : Validation a -> (a -> Validation b) -> Validation b
andThen validation callback =
  \value -> validation value `Result.andThen` (\next -> (callback next) value)


{-| int |> pipeTo (min 5) -}
pipeTo : (a -> Validation b) -> Validation a -> Validation b
pipeTo =
  flip andThen


-- apply : Validation (a -> b) -> Validation a -> Validation b
-- apply f vf =
--   f `andThen` (\f' -> f' `map` vf)

-- (|:) = apply

formatError : (ValidationError -> ValidationError) -> Validation a -> Validation a
formatError f validation =
  \value -> Result.formatError f (validation value)

customError : List String -> Validation a -> Validation a
customError params =
  formatError (\_ -> ValueError (CustomError params))

{-| private -}
groupError : String -> ValidationError -> ValidationError
groupError name e =
  GroupErrors <| Dict.fromList [ (name, e) ]


{-| private -}
err : Error -> Result ValidationError a
err e =
  Err (ValueError e)


{-| private -}
ifErr : Error -> Result e a -> Result ValidationError a
ifErr e res =
  Result.formatError (\_ -> ValueError e) res


{-| get "name" string -}
get : String -> Validation a -> Validation a
get key validation =
  let
    func v = case v of
      Group fields ->
        case Dict.get key fields of
          Just a -> validation a |> Result.formatError (\e -> groupError key e)
          Nothing -> Err (groupError key (ValueError EmptyError))
      _ ->
        Err (groupError key (ValueError EmptyError))
  in
    func


{-| "name" := string -}
(:=) : String -> Validation a -> Validation a
(:=) =
  get


(?=) : String -> Validation a -> Validation (Maybe a)
(?=) s v =
  maybe (get s v)


form1 : (a -> value) -> Validation a -> Validation value
form1 =
  map


form2 : (a -> b -> m) -> Validation a -> Validation b -> Validation m
form2 func v1 v2 value =
  case (v1 value, v2 value) of
    (Ok a, Ok b) ->
      Ok (func a b)
    (r1, r2) ->
      Err (mergeMany [ getErr r1, getErr r2 ])


form3 : (a -> b -> c -> m) -> Validation a -> Validation b -> Validation c -> Validation m
form3 func v1 v2 v3 value =
  case (v1 value, v2 value, v3 value) of
    (Ok a, Ok b, Ok c) ->
      Ok (func a b c)
    (r1, r2, r3) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3 ])


form4 : (a -> b -> c -> d -> m) -> Validation a -> Validation b -> Validation c -> Validation d -> Validation m
form4 func v1 v2 v3 v4 value =
  case (v1 value, v2 value, v3 value, v4 value) of
    (Ok a, Ok b, Ok c, Ok d) ->
      Ok (func a b c d)
    (r1, r2, r3, r4) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3, getErr r4 ])


form5 : (a -> b -> c -> d -> e -> m) -> Validation a -> Validation b -> Validation c -> Validation d -> Validation e -> Validation m
form5 func v1 v2 v3 v4 v5 value =
  case (v1 value, v2 value, v3 value, v4 value, v5 value) of
    (Ok a, Ok b, Ok c, Ok d, Ok e) ->
      Ok (func a b c d e)
    (r1, r2, r3, r4, r5) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3, getErr r4, getErr r5 ])


form6 : (a -> b -> c -> d -> e -> f -> m) -> Validation a -> Validation b -> Validation c -> Validation d -> Validation e -> Validation f -> Validation m
form6 func v1 v2 v3 v4 v5 v6 value =
  case (v1 value, v2 value, v3 value, v4 value, v5 value, v6 value) of
    (Ok a, Ok b, Ok c, Ok d, Ok e, Ok f) ->
      Ok (func a b c d e f)
    (r1, r2, r3, r4, r5, r6) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3, getErr r4, getErr r5, getErr r6 ])


form7 : (a -> b -> c -> d -> e -> f -> g -> m) -> Validation a -> Validation b -> Validation c -> Validation d -> Validation e -> Validation f -> Validation g -> Validation m
form7 func v1 v2 v3 v4 v5 v6 v7 value =
  case (v1 value, v2 value, v3 value, v4 value, v5 value, v6 value, v7 value) of
    (Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g) ->
      Ok (func a b c d e f g)
    (r1, r2, r3, r4, r5, r6, r7) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3, getErr r4, getErr r5, getErr r6, getErr r7 ])


form8 : (a -> b -> c -> d -> e -> f -> g -> h -> m) -> Validation a -> Validation b -> Validation c -> Validation d -> Validation e -> Validation f -> Validation g -> Validation h -> Validation m
form8 func v1 v2 v3 v4 v5 v6 v7 v8 value =
  case (v1 value, v2 value, v3 value, v4 value, v5 value, v6 value, v7 value, v8 value) of
    (Ok a, Ok b, Ok c, Ok d, Ok e, Ok f, Ok g, Ok h) ->
      Ok (func a b c d e f g h)
    (r1, r2, r3, r4, r5, r6, r7, r8) ->
      Err (mergeMany [ getErr r1, getErr r2, getErr r3, getErr r4, getErr r5, getErr r6, getErr r7, getErr r8 ])


mergeMany : List (Maybe ValidationError) -> ValidationError
mergeMany errors =
  errors
    |> List.filterMap identity
    |> List.foldl merge (GroupErrors Dict.empty)


merge : ValidationError -> ValidationError -> ValidationError
merge e1 e2 =
  case (e1, e2) of
    (GroupErrors ge1, GroupErrors ge2) ->
      GroupErrors (Dict.union ge1 ge2)
    _ ->
      e2


getErr : Result e a -> Maybe e
getErr res =
  case res of
    Ok _ -> Nothing
    Err e -> Just e


int : Validation Int
int v =
  case v of
    TextField s -> String.toInt s |> ifErr InvalidInt
    _ -> err InvalidInt


float : Validation Float
float v =
  case v of
    TextField s -> String.toFloat s |> ifErr InvalidFloat
    _ -> err InvalidInt


string : Validation String
string v =
  case v of
    TextField s -> Ok s
    _ -> err InvalidString


bool : Validation Bool
bool v =
  case v of
    CheckBox b -> Ok b
    _ -> Ok False


date : Validation Date
date v =
  case v of
    TextField s -> Date.fromString s |> ifErr InvalidDate
    _ -> err InvalidDate


maybe : Validation a -> Validation (Maybe a)
maybe validation =
  \value -> Ok <| Result.toMaybe (validation value)

--

trim : Validation String -> Validation String
trim validation =
  map String.trim validation


minLength : Int -> String -> Validation String
minLength min s =
  \value -> if String.length s >= min then Ok s else err (ShorterThan min)

nonEmpty : String -> Validation String
nonEmpty =
  minLength 1

{-| "age" := int `andThen` minInt 10 -}
minInt : Int -> Int -> Validation Int
minInt min i =
  \value -> if i >= min then Ok i else err (SmallerThan min)

{-| "age" := int `andThen` minInt 10 -}
maxInt : Int -> Int -> Validation Int
maxInt max i =
  \value -> if i <= max then Ok i else err (GreaterThan max)
