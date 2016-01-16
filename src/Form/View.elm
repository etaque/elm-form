module Form.View where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address)

import Form.Model exposing (..)


updateField : String -> String -> Action
updateField =
  UpdateField

validate : Action
validate =
  Validate

textInput : String -> Form -> Address Action -> List Attribute -> Html
textInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "text"
      , value (getValue form name |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message formAddress (updateField name v))
      ]
  in
    input (formAttrs ++ attrs) []


checkboxInput : String -> Form -> Address Action -> List Attribute -> Html
checkboxInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , value (getValue form name |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message formAddress (updateField name v))
      ]
  in
    input (formAttrs ++ attrs) []


validateOnClick : Signal.Address Action -> Attribute
validateOnClick formAddress =
  onClick formAddress validate


errorsOn : Form -> String -> (FieldError -> String) -> Html
errorsOn form name presenter =
  let
    errors = getErrors form name
  in
    if List.isEmpty errors then
      text ""
    else
      errors
        |> List.map (\s -> span [] [ text (presenter s) ])
        |> div [ class "errors" ]
