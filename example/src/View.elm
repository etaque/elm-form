module View where

import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Form exposing (Form)
import Form.Input

import Model exposing (..)
import View.Bootstrap exposing (..)


view : Signal.Address Action -> Model -> Html
view address {form, userMaybe} =
  let
    formAddress = Signal.forwardTo address FormAction
  in
    div
      [ class "form-vertical"
      , style [ ("margin", "50px auto"), ("width", "600px")]
      ]
      [ legend [] [ text "SimpleForm example" ]
      , row
          [ col' 6
              [ textGroup "Name" (Form.getFieldAsString "name" form) formAddress ]
          , col' 6
              [ textGroup "Email address" (Form.getFieldAsString "email" form) formAddress ]
          ]
      , row
          [ col' 12
              [ checkboxGroup "Administrator" (Form.getFieldAsBool "admin" form) formAddress
              ]
          ]
      , row
          [ col' 4
              [ textGroup "Website" (Form.getFieldAsString "profile.website" form) formAddress ]
          , col' 4
              [ selectGroup (("", "--") :: (List.map (\i -> (i, String.toUpper i)) roles))
                   "Role" (Form.getFieldAsString "profile.role" form) formAddress ]
          , col' 4
              [ radioGroup (List.map (\i -> (i, String.toUpper i)) superpowers)
                "Superpower" (Form.getFieldAsString "profile.superpower" form) formAddress ]
          ]
      , row
          [ col' 6
              [ textGroup "Age" (Form.getFieldAsString "profile.age" form) formAddress ]
          ]
      , row
          [ col' 12
              [ textAreaGroup "Bio" (Form.getFieldAsString "profile.bio" form) formAddress
              ]
          ]
      , hr [] []
      , p
          []
          [ button
              [ onClick formAddress Form.submit
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

      , case Form.getOutput form of
          Just user ->
            p [ class "alert alert-success" ] [ text (toString user) ]
          Nothing ->
            text ""

      , Form.Input.dumpErrors form
      ]







