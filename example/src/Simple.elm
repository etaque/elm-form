module Main exposing (Foo, Model, Msg(..), app, formView, init, update, validate, view)

import Browser
import Form exposing (Form)
import Form.Input as Input
import Form.Validate as Validate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- your expected form output


type alias Foo =
    { bar : String
    , baz : Bool
    }



-- Add form to your model and msgs


type alias Model =
    { form : Form () Foo }


type Msg
    = NoOp
    | FormMsg Form.Msg



-- Setup form validation


init : Model
init =
    { form = Form.initial [] validate }


validate : Validation () Foo
validate =
    succeed Foo
        |> andMap (field "bar" email)
        |> andMap (field "baz" bool)



-- Forward form msgs to Form.update


update : Msg -> Model -> Model
update msg ({ form } as model) =
    case msg of
        NoOp ->
            model

        FormMsg formMsg ->
            { model | form = Form.update validate formMsg form }



-- Render form with Input helpers


view : Model -> Html Msg
view { form } =
    Html.map FormMsg (formView form)


formView : Form () Foo -> Html Form.Msg
formView form =
    let
        -- error presenter
        errorFor field =
            case field.liveError of
                Just error ->
                    -- replace toString with your own translations
                    div [ class "error" ] [ text (Debug.toString error) ]

                Nothing ->
                    text ""

        -- fields states
        bar =
            Form.getFieldAsString "bar" form

        baz =
            Form.getFieldAsBool "baz" form
    in
    div []
        [ label [] [ text "Bar" ]
        , Input.textInput bar []
        , errorFor bar
        , label []
            [ Input.checkboxInput baz []
            , text "Baz"
            ]
        , errorFor baz
        , button
            [ onClick Form.Submit ]
            [ text "Submit" ]
        ]


app =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
