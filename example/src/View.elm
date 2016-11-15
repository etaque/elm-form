module View exposing (..)

import String
import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Form exposing (Form)
import Form.Input as Input
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
            , textGroup (text "Name")
                (Form.getFieldAsString "name" form)
            , textGroup (text "Email address")
                (Form.getFieldAsString "email" form)
            , checkboxGroup (text "Administrator")
                (Form.getFieldAsBool "admin" form)
            , textGroup (text "Website")
                (Form.getFieldAsString "profile.website" form)
            , selectGroup roleOptions
                (text "Role")
                (Form.getFieldAsString "profile.role" form)
            , radioGroup superpowerOptions
                (text "Superpower")
                (Form.getFieldAsString "profile.superpower" form)
            , textGroup (text "Age")
                (Form.getFieldAsString "profile.age" form)
            , textAreaGroup (text "Bio")
                (Form.getFieldAsString "profile.bio" form)
            , todosView form
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


todosView : Form CustomError User -> Html Form.Msg
todosView form =
    let
        allTodos =
            List.concatMap (todoItemView form) (Form.getListIndexes "todos" form)
    in
        div
            [ class "row" ]
            [ col' 3
                [ label [ class "control-label" ] [ text "Todolist" ]
                , br [] []
                , button [ onClick (Form.Append "todos"), class "btn btn-xs btn-default" ] [ text "Add" ]
                ]
            , col' 9
                [ div [ class "todos" ] allTodos
                ]
            ]


todoItemView : Form CustomError User -> Int -> List (Html Form.Msg)
todoItemView form i =
    let
        labelField =
            Form.getFieldAsString ("todos." ++ (toString i) ++ ".label") form
    in
        [ div
            [ class ("input-group" ++ (errorClass labelField.liveError)) ]
            [ span
                [ class "input-group-addon" ]
                [ Input.checkboxInput
                    (Form.getFieldAsBool ("todos." ++ (toString i) ++ ".done") form)
                    []
                ]
            , Input.textInput
                labelField
                [ class "form-control" ]
            , span
                [ class "input-group-btn" ]
                [ button
                    [ onClick (Form.RemoveItem "todos" i), class "btn btn-danger" ]
                    [ text "Remove" ]
                ]
            ]
        , br [] []
        ]
