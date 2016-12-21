module View.Bootstrap exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Form exposing (Form, FieldState)
import Form.Input as Input
import Form.Field as Field
import Form.Error exposing (Error, ErrorValue)
import Model exposing (..)


row : List (Html Form.Msg) -> Html Form.Msg
row content =
    div [ class "row" ] content


colN : Int -> List (Html Form.Msg) -> Html Form.Msg
colN i content =
    div [ class ("col-xs-" ++ toString i) ] content


type alias GroupBuilder a =
    Html Form.Msg -> FieldState CustomError a -> Html Form.Msg


formGroup : Html Form.Msg -> Maybe (ErrorValue CustomError) -> List (Html Form.Msg) -> Html Form.Msg
formGroup label_ maybeError inputs =
    div
        [ class ("row form-group " ++ (errorClass maybeError)) ]
        [ colN 3
            [ label [ class "control-label" ] [ label_ ] ]
        , colN 5
            inputs
        , colN 4
            [ errorMessage maybeError ]
        ]


formActions : List (Html Form.Msg) -> Html Form.Msg
formActions content =
    row
        [ div [ class "col-xs-offset-3 col-xs-9" ] content ]


textGroup : GroupBuilder String
textGroup label_ state =
    formGroup label_
        state.liveError
        [ Input.textInput state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]


dateGroup : GroupBuilder String
dateGroup label_ state =
    formGroup label_
        state.liveError
        [ Input.baseInput "date"
            Field.String
            Form.Text
            state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            , placeholder "yyyy-mm-dd"
            ]
        ]


textAreaGroup : GroupBuilder String
textAreaGroup label_ state =
    formGroup label_
        state.liveError
        [ Input.textArea state
            [ class "form-control"
            , value (Maybe.withDefault "" state.value)
            ]
        ]


checkboxGroup : GroupBuilder Bool
checkboxGroup label_ state =
    formGroup (text "")
        state.liveError
        [ div
            [ class "checkbox" ]
            [ label []
                [ Input.checkboxInput state []
                , label_
                ]
            ]
        ]


selectGroup : List ( String, String ) -> GroupBuilder String
selectGroup options label_ state =
    formGroup label_
        state.liveError
        [ Input.selectInput options state [ class "form-control" ] ]


radioGroup : List ( String, String ) -> GroupBuilder String
radioGroup options label_ state =
    let
        item ( v, l ) =
            label
                [ class "radio-inline" ]
                [ Input.radioInput v state []
                , text l
                ]
    in
        formGroup label_
            state.liveError
            (List.map item options)


errorClass : Maybe error -> String
errorClass maybeError =
    Maybe.map (\_ -> "has-error") maybeError |> Maybe.withDefault ""


errorMessage : Maybe (ErrorValue CustomError) -> Html Form.Msg
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
