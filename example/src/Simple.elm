module Main exposing (..)

import Html.App as Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Form exposing (Form)
import Form.Validate as Validate exposing (..)
import Form.Input as Input


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


init : ( Model, Cmd Msg )
init =
    ( { form = Form.initial [] validate }, Cmd.none )


validate : Validation () Foo
validate =
    form2 Foo
        (get "bar" email)
        (get "baz" bool)



-- Forward form msgs to Form.update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ form } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FormMsg formMsg ->
            ( { model | form = Form.update formMsg form }, Cmd.none )



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
                    div [ class "error" ] [ text (toString error) ]

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
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
