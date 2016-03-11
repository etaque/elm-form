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
    roleOptions =
      ("", "--") :: (List.map (\i -> (i, String.toUpper i)) roles)
    superpowerOptions =
      List.map (\i -> (i, String.toUpper i)) superpowers
    submitClick =
      case Form.getOutput form of
        Just user ->
          onClick address (SubmitUser user)
        Nothing ->
          onClick formAddress Form.Submit
  in
    div
      [ class "form-horizontal"
      , style [ ("margin", "50px auto"), ("width", "600px")]
      ]
      [ legend [] [ text "Elm Simple Form example" ]

      , textGroup "Name" formAddress
          (Form.getFieldAsString "name" form)

      , textGroup "Email address" formAddress
          (Form.getFieldAsString "email" form)

      , checkboxGroup "Administrator" formAddress
          (Form.getFieldAsBool "admin" form)

      , textGroup "Website" formAddress
          (Form.getFieldAsString "profile.website" form)

      , selectGroup roleOptions "Role" formAddress
          (Form.getFieldAsString "profile.role" form)

      , radioGroup superpowerOptions "Superpower" formAddress
          (Form.getFieldAsString "profile.superpower" form)

      , textGroup "Age" formAddress
          (Form.getFieldAsString "profile.age" form)

      , textAreaGroup "Bio" formAddress
          (Form.getFieldAsString "profile.bio" form)

      , formActions
          [ button
              [ submitClick
              , class "btn btn-primary"
              ]
              [ text "Submit" ]
          , text " "
          , button
              [ onClick formAddress (Form.Reset initialFields)
              , class "btn btn-default"
              ]
              [ text "Reset" ]
          ]

      , case userMaybe of
          Just user ->
            p [ class "alert alert-success" ] [ text (toString user) ]
          Nothing ->
            text ""

      -- , hr [] []
      -- , Form.Input.dumpErrors form
      ]







