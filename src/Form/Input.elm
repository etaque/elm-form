module Form.Input
  ( textInput, checkboxInput, selectInput
  , liveErrorAt
  ) where

import Signal exposing (Address)
import Maybe exposing (andThen)
import String

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Form.Error as Error exposing (Error)
import Form exposing (Form, Action)



-- input helpers

textInput : String -> Form e o -> Address Action -> List Attribute -> Html
textInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "text"
      , value (Form.getStringAt name form |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message formAddress (Form.updateTextField name v))
      , onBlur formAddress Form.validate
      ]
  in
    input (formAttrs ++ attrs) []


selectInput : List (String, String) -> String -> Form e o -> Address Action -> List Attribute -> Html
selectInput options name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , on "change"
          targetValue
          (\v -> Signal.message formAddress (Form.updateSelectField name v))
      , onBlur formAddress Form.validate
      ]
    currentValue = Form.getStringAt name form
    isSelected k =
      case currentValue of
        Just k' -> k' == k
        Nothing -> False
    buildOption (k, v) =
      option [ value k, selected (isSelected k) ] [ text v ]
  in
    select (formAttrs ++ attrs) <|
      List.map buildOption options


checkboxInput : String -> Form e o -> Address Action -> List Attribute -> Html
checkboxInput name form formAddress attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , checked (Form.getBoolAt name form |> Maybe.withDefault False)
      , on "change"
          targetChecked
          (\v -> Signal.message formAddress (Form.updateCheckField name v))
      , onBlur formAddress Form.validate
      ]
  in
    input (formAttrs ++ attrs) []


-- radioInput : String -> String -> Form e o -> Address Action -> List Attribute -> Html
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


liveErrorAt : String -> Form e o -> Maybe (Error e)
liveErrorAt name form =
  if Form.isSubmitted form ||
      (Form.isVisitedAt name form && not (Form.isDirtyAt name form)) then
    Form.getErrorAt name form
  else
    Nothing

