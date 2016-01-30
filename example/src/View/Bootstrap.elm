module View.Bootstrap where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Error exposing (Error)

import Model exposing (..)

row : List Html -> Html
row content =
  div [ class "row" ] content

col' : Int -> List Html -> Html
col' i content =
  div [ class ("col-xs-" ++ toString i) ] content


type alias GroupBuilder a = String -> FieldState CustomError a -> Signal.Address Form.Action -> Html


textGroup : GroupBuilder String
textGroup label' state address =
  div
    [ class ("form-group " ++ errorClass state.liveError) ]
    [ label [] [ text label' ]
    , Input.textInput state address [ class "form-control" ]
    , errorMessage state.liveError
    ]


textAreaGroup : GroupBuilder String
textAreaGroup label' state address =
  div
    [ class ("form-group " ++ errorClass state.liveError) ]
    [ label [] [ text label' ]
    , Input.textArea state address [ class "form-control" ]
    , errorMessage state.liveError
    ]


checkboxGroup : GroupBuilder Bool
checkboxGroup label' state address =
  div
    [ class ("form-group " ++ errorClass state.liveError) ]
    [ div
        [ class "checkbox" ]
        [ label []
            [ Input.checkboxInput state address []
            , text label'
            ]
        ]
    ]


selectGroup : List (String, String) -> GroupBuilder String
selectGroup options label' state address =
  div
    [ class ("form-group " ++ errorClass state.liveError) ]
    [ label [] [ text label' ]
    , Input.selectInput options state address [ class "form-control" ]
    , errorMessage state.liveError
    ]


radioGroup : List (String, String) -> GroupBuilder String
radioGroup options label' state address =
  let
    item (v, l) =
      label
        [ class "radio-inline" ]
        [ Input.radioInput state.path state address [ value v ]
        , text l
        ]
  in
    div
      [ class ("form-group " ++ errorClass state.liveError) ]
      [ label [] [ text label' ]
      , div [] (List.map item options)
      , errorMessage state.liveError
      ]


-- formGroup : String -> Maybe (Error CustomError) -> Html -> Html
-- formGroup label' maybeError field =
--   div
--     [ class ("form-group " ++ (errorClass maybeError)) ]
--     [ label [ class "control-label" ] [ text label' ]
--     , field
--     , errorMessage maybeError
--     ]


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
