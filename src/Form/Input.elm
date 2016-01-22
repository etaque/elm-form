module Form.Input
  ( textInput, checkboxInput, selectInput
  , validateOnClick, errorMessage
  ) where

import Signal exposing (Address)
import Maybe exposing (andThen)
import String

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- import Form.Core as Core exposing (..)
import Form.Value as Value exposing (..)
import Form.Error as Error exposing (..)
import Form exposing (..)



-- input helpers

textInput : String -> Form e t a -> Address Action -> List Attribute -> Html
textInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "text"
      , value (getStringAt name form |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message formAddress (updateStringAt name v))
      ]
  in
    input (formAttrs ++ attrs) []


selectInput : List (String, String) -> String -> Form e t a -> Address Action -> List Attribute -> Html
selectInput options name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , on "change"
          targetValue
          (\v -> Signal.message formAddress (updateStringAt name v))
      ]
    currentValue = getStringAt name form
    isSelected k =
      case currentValue of
        Just k' -> k' == k
        Nothing -> False
    buildOption (k, v) =
      option [ value k, selected (isSelected k) ] [ text v ]
  in
    select (formAttrs ++ attrs) <|
      List.map buildOption options


checkboxInput : String -> Form e t a -> Address Action -> List Attribute -> Html
checkboxInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , checked (getBoolAt name form |> Maybe.withDefault False)
      , on "change"
          targetChecked
          (\v -> Signal.message formAddress (updateBoolAt name v))
      ]
  in
    input (formAttrs ++ attrs) []


-- radioInput : String -> String -> Form e t a -> Address Action -> List Attribute -> Html
-- radioInput value name form formAddress attrs =
--   le
--     formAttrs =
--       [ type' "radio"
--       , checked ((getValue form |> getField name) `andThen` getBool |> Maybe.withDefault False)
--       , on "change"
--           targetValue
--           (\v -> Signal.message formAddress (updateString name v))
--       ]
--   in
--     input (formAttrs ++ attrs) []


validateOnClick : Signal.Address Action -> Attribute
validateOnClick formAddress =
  onClick formAddress validate


errorMessage : Form e t a -> String -> (Error e -> String) -> Html
errorMessage form name presenter =
  case getErrorAt name form of
    Just error ->
      div [ class "errors" ] [ text (presenter error) ]
    Nothing ->
      text ""
