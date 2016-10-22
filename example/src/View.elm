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
            , linksView form
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


linksView : Form CustomError User -> Html Form.Msg
linksView form =
    let
        linkView i =
            div
                [ class "row link" ]
                [ col' 3 [ text "Link" ]
                , Input.textInput
                    (Form.getFieldAsString ("links." ++ (toString i) ++ ".name") form)
                    [ placeholder "Name" ]
                , textGroup
                    "URL"
                    (Form.getFieldAsString ("links." ++ (toString i) ++ ".url") form)
                , button [ onClick (Form.RemoveItem "links" i) ] [ text "Remove" ]
                ]
    in
        div
            [ class "links" ]
        <|
            (List.map linkView (Form.getListIndexes "links" form))
                ++ [ button [ onClick (Form.Append "links") ] [ text "Add link" ] ]
