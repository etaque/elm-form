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
              [ textGroup "Name" (Form.getFieldAsString "name" form) formAddress ]
          , col' 6
              [ selectGroup [ ("", "--"), ("a", "Option A"), ("b", "Option B") ]
                   "Role" (Form.getFieldAsString "role" form) formAddress ]
          ]
      , row
          [ col' 6
              [ textGroup "Age" (Form.getFieldAsString "age" form) formAddress ]
          , col' 6
              [ textGroup "Email address" (Form.getFieldAsString "email" form) formAddress ]
          ]
      , row
          [ col' 6
            [ checkboxGroup "Administrator" (Form.getFieldAsBool "admin" form) formAddress ]
          , col' 6
            [ radioGroup (List.map (\i -> (i, String.toUpper i)) foos)
                "Foo" (Form.getFieldAsString "profile.foo" form) formAddress ]
          ]
      , row
          [ col' 12
            [ textAreaGroup "Bar" (Form.getFieldAsString "profile.bar" form) formAddress ]
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







