module Form.View where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Signal exposing (Address)
import Maybe exposing (andThen)

import Form.Model exposing (..)


validate : Action
validate =
  Validate

textInput : String -> Form t a -> Address Action -> List Attribute -> Html
textInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "text"
      , value ((getValue form |> getField name) `andThen` getString |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message formAddress (updateString name v))
      ]
  in
    input (formAttrs ++ attrs) []


checkboxInput : String -> Form t a -> Address Action -> List Attribute -> Html
checkboxInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , checked ((getValue form |> getField name) `andThen` getBool |> Maybe.withDefault False)
      , on "change"
          targetChecked
          (\v -> Signal.message formAddress (updateBool name (Debug.log "checked" v)))
      ]
  in
    input (formAttrs ++ attrs) []


validateOnClick : Signal.Address Action -> Attribute
validateOnClick formAddress =
  onClick formAddress validate


errorsOn : Form t a-> String -> (Error -> String) -> Html
errorsOn form name presenter =
  let
    errors = getErrors name form
  in
    if List.isEmpty errors then
      text ""
    else
      errors
        |> List.map (\s -> span [] [ text (presenter s) ])
        |> div [ class "errors" ]
