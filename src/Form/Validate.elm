module Form.Validate
  ( FieldError, get, maybe, map, andThen
  , string, int, float, bool, date
  , min
  ) where

import Result
import Date exposing (Date)
import Time exposing (Time)
import Dict exposing (Dict)
import String

import Form.Model exposing (..)


type alias FieldError = Form.Model.FieldError

get : String -> (FieldValue -> ValidationResult a) -> FieldValidator a
get name validate =
  let
    validateMaybe maybeString =
      case maybeString of
        Just s ->
          if String.isEmpty s then
            Err Empty
          else
            validate s
        Nothing ->
          Err Empty
      -- Maybe.map validate maybeValue
      --   |> Maybe.withDefault (Err Empty)
  in
    FieldValidator name validateMaybe


maybe : String -> (FieldValue -> ValidationResult a) -> FieldValidator (Maybe a)
maybe name validate =
  let
    validateMaybe maybeValue =
      case maybeValue of
        Just s ->
          validate s |> Result.map Just
        Nothing ->
          Ok Nothing
  in
    FieldValidator name validateMaybe

map : (a -> b) -> (FieldValue -> ValidationResult a) -> (FieldValue -> ValidationResult b)
map f validation =
  (\value -> Result.map f (validation value))

andThen : (FieldValue -> ValidationResult a) -> (a -> ValidationResult b) -> (FieldValue -> ValidationResult b)
andThen validation callback =
  (\value -> validation value `Result.andThen` callback)


--


string : FieldValue -> ValidationResult String
string =
  Ok


int : FieldValue -> ValidationResult Int
int value =
  String.toInt value
    |> Result.formatError (\_ -> InvalidInt)


float : FieldValue -> ValidationResult Float
float value =
  String.toFloat value
    |> Result.formatError (\_ -> InvalidInt)


bool : FieldValue -> ValidationResult Bool
bool value =
  Ok True


min : Int -> Int -> ValidationResult Int
min i value =
  if value >= i then
    Ok value
  else
    Err (LowerThan i)

date : FieldValue -> ValidationResult Date
date value =
  Date.fromString value
    |> Result.formatError (\_ -> InvalidDate)






