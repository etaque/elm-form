module View where

import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
import Form.Input as Input

import Model exposing (..)
import View.Bootstrap exposing (..)


view : Signal.Address Action -> Model -> Html
view address {form, userMaybe} =
  let
    formAddress = Signal.forwardTo address FormAction
    submitOnClick =
      case Form.getOutput form of
        Just user ->
          onClick address (SubmitUser user)
        Nothing ->
          onClick formAddress Form.submit
  in
    div
      [ class "form-vertical"
      , style [ ("margin", "50px auto"), ("width", "600px")]
      ]
      [ row
          [ col' 6
              [ textGroup "name" "Name" form formAddress ]
          , col' 6
              [ selectGroup [ ("", "--"), ("a", "Option A"), ("b", "Option B") ]
                  "role" "Role" form formAddress ]
          ]
      , row
          [ col' 6
              [ textGroup "age" "Age" form formAddress ]
          , col' 6
              [ textGroup "email" "Email address" form formAddress ]
          ]
      , row
          [ col' 6
            [ checkboxGroup "admin" "Administrator" form formAddress ]
          , col' 6
            [ radioGroup (List.map (\i -> (i, String.toUpper i)) foos)
                "profile.foo" "Foo" form formAddress ]
          ]
      , row
          [ col' 12
            [ textAreaGroup "profile.bar" "Bar" form formAddress ]
          ]
      , div
          []
          [ button
              [ submitOnClick
              , class "btn btn-primary"
              ]
              [ text "Submit" ]
          , text " "
          , button
              [ onClick formAddress (Form.reset initialFields)
              , class "btn btn-default"
              ]
              [ text "Reset" ]
          ]

      , hr [] []
      , text (toString userMaybe)
      , Input.dumpErrors form
      ]







