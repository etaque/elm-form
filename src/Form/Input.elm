module Form.Input
  ( Input, textInput, checkboxInput, selectInput, radioInput, radioGroup
  , liveErrorAt
  ) where

import Signal exposing (Address)
import Maybe exposing (andThen)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HtmlAttr exposing (..)

import Form.Error as Error exposing (Error)
import Form exposing (Form, Action)


{-| An input render Html from a field name, a form and address for actions. -}
type alias Input e o = String -> Form e o -> Address Action -> List Attribute -> Html

-- input helpers

textInput : Input e o
textInput name form addr attrs =
  let
    formAttrs =
      [ type' "text"
      , value (Form.getStringAt name form |> Maybe.withDefault "")
      , on "input"
          targetValue
          (\v -> Signal.message addr (Form.updateTextField name v))
      , onBlur addr Form.validate
      ]
  in
    input (formAttrs ++ attrs) []


selectInput : List (String, String) -> Input e o
selectInput options name form addr attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , on "change"
          targetValue
          (\v -> Signal.message addr (Form.updateSelectField name v))
      , onBlur addr Form.validate
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


checkboxInput : Input e o
checkboxInput name form addr attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , checked (Form.getBoolAt name form |> Maybe.withDefault False)
      , on "change"
          targetChecked
          (\v -> Signal.message addr (Form.updateCheckField name v))
      , onBlur addr Form.validate
      ]
  in
    input (formAttrs ++ attrs) []


radioInput : String -> Input e o
radioInput value name form addr attrs =
  let
    formAttrs =
      [ type' "radio"
      , HtmlAttr.name name
      , checked (Form.getStringAt name form == Just value)
      , on "change"
          targetValue
          (\v -> Signal.message addr (Form.updateRadioField name v))
      ]
  in
    input (formAttrs ++ attrs) []


radioGroup : List (String, String) -> List Attribute -> Input e o
radioGroup options groupAttrs name form addr attrs =
  let
    item (v, l) =
      label [ for v ] [ text l, radioInput v name form addr attrs ]
  in
    div groupAttrs (List.map item options)



liveErrorAt : String -> Form e o -> Maybe (Error e)
liveErrorAt name form =
  if Form.isSubmitted form ||
      (Form.isVisitedAt name form && not (Form.isDirtyAt name form)) then
    Form.getErrorAt name form
  else
    Nothing

