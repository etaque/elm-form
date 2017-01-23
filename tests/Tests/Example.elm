module Tests.Example exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Model
import Form exposing (Form)
import Form.Error exposing (..)
import Form.Field as Field


all : Test
all =
    describe "Initial example validation"
        [ test "has no output" <|
            \_ -> equal Nothing (Form.getOutput validatedForm)
        , test "has errors on expected fields" <|
            \_ ->
                equal
                    (Form.getErrors validatedForm)
                    [ ( "date", InvalidDate )
                    , ( "email", InvalidString )
                    , ( "profile.role", InvalidString )
                    , ( "profile.superpower", InvalidString )
                    ]
        , test "append, set then get field in list" <|
            \_ ->
                let
                    ( name, value ) =
                        ( "links.0.name", "Twitter" )

                    formAfterAppend =
                        Form.update Model.validate (Form.Append "links") initialForm

                    formAfterInput =
                        Form.update Model.validate (Form.Input name Form.Text (Field.String value)) formAfterAppend

                    maybeState =
                        Form.getFieldAsString name formAfterInput
                in
                    equal (Just value) maybeState.value
        ]


initialForm : Form Model.CustomError Model.User
initialForm =
    Form.initial Model.initialFields Model.validate


validatedForm : Form Model.CustomError Model.User
validatedForm =
    Form.update Model.validate Form.Validate initialForm
