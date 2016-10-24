module View.Bootstrap exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Error exposing (Error)
import Model exposing (..)


row : List (Html Form.Msg) -> Html Form.Msg
row content =
    div [ class "row" ] content


col' : Int -> List (Html Form.Msg) -> Html Form.Msg
col' i content =
    div [ class ("col-xs-" ++ toString i) ] content


type alias GroupBuilder a =
    String -> FieldState CustomError a -> Html Form.Msg


formGroup : String -> Maybe (Error CustomError) -> List (Html Form.Msg) -> Html Form.Msg
formGroup label' maybeError inputs =
    div
        [ class ("row form-group " ++ (errorClass maybeError)) ]
        [ col' 3
            [ label [ class "control-label" ] [ text label' ] ]
        , col' 5
            inputs
        , col' 4
            [ errorMessage maybeError ]
        ]


formActions : List (Html Form.Msg) -> Html Form.Msg
formActions content =
    row
        [ div [ class "col-xs-offset-3 col-xs-9" ] content ]


textGroup : GroupBuilder String
textGroup label' state =
    formGroup label'
        state.liveError
        [ Input.textInput state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]


textAreaGroup : GroupBuilder String
textAreaGroup label' state =
    formGroup label'
        state.liveError
        [ Input.textArea state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]


checkboxGroup : GroupBuilder Bool
checkboxGroup label' state =
    formGroup ""
        state.liveError
        [ div
            [ class "checkbox" ]
            [ label []
                [ Input.checkboxInput state []
                , text label'
                ]
            ]
        ]


selectGroup : List ( String, String ) -> GroupBuilder String
selectGroup options label' state =
    formGroup label'
        state.liveError
        [ Input.selectInput options state [ class "form-control" ] ]


radioGroup : List ( String, String ) -> GroupBuilder String
radioGroup options label' state =
    let
        item ( v, l ) =
            label
                [ class "radio-inline" ]
                [ Input.radioInput v state []
                , text l
                ]
    in
        formGroup label'
            state.liveError
            (List.map item options)


errorClass : Maybe error -> String
errorClass maybeError =
    Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


errorMessage : Maybe (Error CustomError) -> Html Form.Msg
errorMessage maybeError =
    case maybeError of
        Just error ->
            p
                [ class "help-block" ]
                [ text (toString error) ]

        Nothing ->
            span
                [ class "help-block" ]
                [ text "\x2007" ]



-- &#8199;
