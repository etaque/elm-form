module Form.Validate
  ( Error, Validation, get, map, andThen
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


-- formatError : (e -> e') -> Validation a -> Validation a
-- formatError f validation =
--   \value -> Result.formatError f (validation value)


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


form2 : (a -> b -> value) -> Validation a -> Validation b -> Validation value
form2 f v1 v2 =
  (\value -> Result.map2 f (v1 value) (v2 value)) -- TODO merge errors


form3 : (a -> b -> c -> value) -> Validation a -> Validation b -> Validation c -> Validation value
form3 f v1 v2 v3 =
  (\value -> Result.map3 f (v1 value) (v2 value) (v3 value)) -- TODO merge errors


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
