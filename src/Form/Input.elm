module Form.Input (Input, baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput, dumpErrors) where

{-|
@docs Input

@docs baseInput, textInput, passwordInput, textArea, checkboxInput, selectInput, radioInput

@docs dumpErrors
-}

import Signal exposing (Address)
import Maybe exposing (andThen)
import String
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HtmlAttr exposing (..)
import Form exposing (Form, Action, FieldState, Action (OnInput, OnFocus, OnBlur))
import Form.Field exposing (Field(..))


{-| An input render Html from a field state, a form and address for actions.
All input functions using this type alias are pre-wired with event handlers.
-}
type alias Input e a =
  FieldState e a -> Address Action -> List Attribute -> Html


(?=) =
  flip Maybe.withDefault


{-| Untyped input, first param is `type` attribute.
-}
baseInput : String -> (String -> Field) -> Input e String
baseInput t toField state addr attrs =
  let
    formAttrs =
      [ type' t
      , value (state.value ?= "")
      , on
          "input"
          targetValue
          (\v -> Signal.message addr (OnInput state.path (toField v)))
      , onFocus addr (OnFocus state.path)
      , onBlur addr (OnBlur state.path)
      ]
  in
    input (formAttrs ++ attrs) []


{-| Text input.
-}
textInput : Input e String
textInput =
  baseInput "text" Text


{-| Password input.
-}
passwordInput : Input e String
passwordInput =
  baseInput "password" Text


{-| Textarea.
-}
textArea : Input e String
textArea state addr attrs =
  let
    formAttrs =
      [ on
          "input"
          targetValue
          (\v -> Signal.message addr (OnInput state.path (Textarea v)))
      , onFocus addr (OnFocus state.path)
      , onBlur addr (OnBlur state.path)
      ]

    value =
      state.value ?= ""
  in
    Html.textarea (formAttrs ++ attrs) [ text value ]


{-| Select input.
-}
selectInput : List ( String, String ) -> Input e String
selectInput options state addr attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , on
          "change"
          targetValue
          (\v -> Signal.message addr (OnInput state.path (Select v)))
      , onFocus addr (OnFocus state.path)
      , onBlur addr (OnBlur state.path)
      ]

    buildOption ( k, v ) =
      option [ value k, selected (state.value == Just k) ] [ text v ]
  in
    select (formAttrs ++ attrs) (List.map buildOption options)


{-| Checkbox input.
-}
checkboxInput : Input e Bool
checkboxInput state addr attrs =
  let
    formAttrs =
      [ type' "checkbox"
      , checked (state.value ?= False)
      , on
          "change"
          targetChecked
          (\v -> Signal.message addr (OnInput state.path (Check v)))
      , onFocus addr (OnFocus state.path)
      , onBlur addr (OnBlur state.path)
      ]
  in
    input (formAttrs ++ attrs) []


{-| Radio input.
-}
radioInput : String -> Input e String
radioInput value state addr attrs =
  let
    formAttrs =
      [ type' "radio"
      , HtmlAttr.name value
      , checked (state.value == Just value)
      , onFocus addr (OnFocus state.path)
      , onBlur addr (OnBlur state.path)
      , on
          "change"
          targetValue
          (\v -> Signal.message addr (OnInput state.path (Radio v)))
      ]
  in
    input (formAttrs ++ attrs) []


{-| Dump all form errors in a `<pre>` tag. Useful for debugging.
-}
dumpErrors : Form e o -> Html
dumpErrors form =
  let
    line ( name, error ) =
      name ++ ": " ++ (toString error)

    content =
      Form.getErrors form |> List.map line |> String.join "\n"
  in
    pre [] [ text content ]
