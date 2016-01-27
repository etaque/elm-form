module View.Bootstrap where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
import Form.Input as Input
import Form.Error exposing (Error)

import Model exposing (..)

row : List Html -> Html
row content =
  div [ class "row" ] content

col' : Int -> List Html -> Html
col' i content =
  div [ class ("col-xs-" ++ toString i) ] content


textGroup : String -> String -> Form CustomError a -> Signal.Address Form.Action -> Html
textGroup key label' form address =
  div
    [ class ("form-group " ++ errorClass (Input.liveErrorAt key form)) ]
    [ label [] [ text label' ]
    , Input.textInput key form address [ class "form-control" ]
    , errorMessage (Input.liveErrorAt key form)
    ]


textAreaGroup : String -> String -> Form CustomError a -> Signal.Address Form.Action -> Html
textAreaGroup key label' form address =
  div
    [ class ("form-group " ++ errorClass (Input.liveErrorAt key form)) ]
    [ label [] [ text label' ]
    , Input.textArea key form address [ class "form-control" ]
    , errorMessage (Input.liveErrorAt key form)
    ]


checkboxGroup : String -> String -> Form CustomError a -> Signal.Address Form.Action -> Html
checkboxGroup key label' form address =
  div
    [ class ("form-group " ++ errorClass (Input.liveErrorAt key form)) ]
    [ div
        [ class "checkbox" ]
        [ label []
            [ Input.checkboxInput key form address []
            , text key
            ]
        ]
    ]


selectGroup : List (String, String) -> String -> String -> Form CustomError a -> Signal.Address Form.Action -> Html
selectGroup options key label' form address =
  div
    [ class ("form-group " ++ errorClass (Input.liveErrorAt key form)) ]
    [ label [] [ text label' ]
    , Input.selectInput options key form address [ class "form-control" ]
    , errorMessage (Input.liveErrorAt key form)
    ]


radioGroup : List (String, String) -> String -> String -> Form CustomError a -> Signal.Address Form.Action -> Html
radioGroup options key label' form address =
  let
    item (v, l) =
      label
        [ for v, class "radio-inline" ]
        [ Input.radioInput v key form address []
        , text l
        ]
  in
    div
      [ class ("form-group " ++ errorClass (Input.liveErrorAt key form)) ]
      [ label [] [ text label' ]
      , div [] (List.map item options)
      , errorMessage (Input.liveErrorAt key form)
      ]


formGroup : String -> Maybe (Error CustomError) -> Html -> Html
formGroup label' maybeError field =
  div
    [ class ("form-group " ++ (errorClass maybeError)) ]
    [ label [ class "control-label" ] [ text label' ]
    , field
    , errorMessage maybeError
    ]


errorClass : Maybe error -> String
errorClass maybeError =
  Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


errorMessage : Maybe (Error CustomError) -> Html
errorMessage maybeError =
  case maybeError of
    Just error ->
      span
        [ class "help-block" ]
        [ text (toString error) ]
    Nothing ->
      text ""
