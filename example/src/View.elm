module View exposing (..)

import String
import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Form exposing (Form)
import Model exposing (..)
import View.Bootstrap exposing (..)


view : Model -> Html Msg
view { form, userMaybe } =
    div
        []
        [ Html.map FormMsg (formView form)
        , case userMaybe of
            Just user ->
                p [ class "alert alert-success" ] [ text (toString user) ]

            Nothing ->
                text ""
        ]


formView : Form CustomError User -> Html Form.Msg
formView form =
    let
        roleOptions =
            ( "", "--" ) :: (List.map (\i -> ( i, String.toUpper i )) roles)

        superpowerOptions =
            List.map (\i -> ( i, String.toUpper i )) superpowers
    in
        div
            [ class "form-horizontal"
            , style [ ( "margin", "50px auto" ), ( "width", "600px" ) ]
            ]
            [ legend [] [ text "Elm Simple Form example" ]
            , textGroup "Name"
                (Form.getFieldAsString "name" form)
            , textGroup "Email address"
                (Form.getFieldAsString "email" form)
            , checkboxGroup "Administrator"
                (Form.getFieldAsBool "admin" form)
            , textGroup "Website"
                (Form.getFieldAsString "profile.website" form)
            , selectGroup roleOptions
                "Role"
                (Form.getFieldAsString "profile.role" form)
            , radioGroup superpowerOptions
                "Superpower"
                (Form.getFieldAsString "profile.superpower" form)
            , textGroup "Age"
                (Form.getFieldAsString "profile.age" form)
            , textAreaGroup "Bio"
                (Form.getFieldAsString "profile.bio" form)
            , formActions
                [ button
                    [ onClick Form.Submit
                    , class "btn btn-primary"
                    ]
                    [ text "Submit" ]
                , text " "
                , button
                    [ onClick (Form.Reset initialFields)
                    , class "btn btn-default"
                    ]
                    [ text "Reset" ]
                ]
            ]
