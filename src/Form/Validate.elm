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
    validateMaybe maybeValue =
      case maybeValue of
        Just v ->
          validate v
        _ ->
          Err Empty
  in
    FieldValidator name validateMaybe


maybe : String -> (FieldValue -> ValidationResult a) -> FieldValidator (Maybe a)
maybe name validate =
  let
    validateMaybe maybeValue =
      case maybeValue of
        Just v ->
          validate v |> Result.map Just
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
string v =
  case v of
    Text s ->
      Ok s
    _ ->
      Err InvalidString


int : FieldValue -> ValidationResult Int
int v =
  case v of
    Text s ->
      String.toInt s
        |> Result.formatError (\_ -> InvalidInt)
    _ ->
      Err InvalidInt


float : FieldValue -> ValidationResult Float
float v =
  case v of
    Text s ->
      String.toFloat s
        |> Result.formatError (\_ -> InvalidInt)
    _ ->
      Err InvalidFloat


bool : FieldValue -> ValidationResult Bool
bool v =
  case v of
    Check b ->
      Ok b
    _ ->
      Err InvalidBool


min : Int -> Int -> ValidationResult Int
min i value =
  if value >= i then
    Ok value
  else
    Err (LowerThan i)

date : FieldValue -> ValidationResult Date
date v =
  case v of
    Text s ->
      Date.fromString s
        |> Result.formatError (\_ -> InvalidDate)
    _ ->
      Err InvalidDate






