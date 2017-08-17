module Tests.ChangedFields exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Model
import Form exposing (Form)
import Form.Field as Field
import Set exposing (Set)


all : Test
all =
    describe "Test Changed fields function"
        [ test "one field changed" <|
            \_ ->
                let
                    ( name, value ) =
                        ( "name", "John Doe" )

                    formAfterInput =
                        formStringInput name value initialForm

                    changedFields =
                        Form.getChangedFields formAfterInput

                    expected =
                        Set.insert name Set.empty
                in
                    equal expected changedFields
        , test "field changed and change reverted back" <|
            \_ ->
                let
                    ( name, value ) =
                        ( "name", "John Doe" )

                    formAfterInput =
                        formStringInput name value initialForm
                            |> formStringInput name "hey"

                    changedFields =
                        Form.getChangedFields formAfterInput
                in
                    equal Set.empty changedFields
        , test "checkbox field changed" <|
            \_ ->
                let
                    name =
                        "admin"

                    formAfterInput =
                        formBoolInput name True initialForm

                    changedFields =
                        Form.getChangedFields formAfterInput

                    expected =
                        Set.insert name Set.empty
                in
                    equal expected changedFields
        , test "checkbox field checked and unchecked" <|
            \_ ->
                let
                    name =
                        "admin"

                    formAfterInput =
                        formBoolInput name True initialForm
                            |> formBoolInput name False

                    changedFields =
                        Form.getChangedFields formAfterInput
                in
                    equal Set.empty changedFields
        ]


formBoolInput : String -> Bool -> Form Model.CustomError Model.User -> Form Model.CustomError Model.User
formBoolInput name value form =
    Form.update Model.validate (Form.Input name Form.Checkbox (Field.Bool value)) form


formStringInput : String -> String -> Form Model.CustomError Model.User -> Form Model.CustomError Model.User
formStringInput name value form =
    Form.update Model.validate (Form.Input name Form.Text (Field.String value)) form


initialForm : Form Model.CustomError Model.User
initialForm =
    Form.initial Model.initialFields Model.validate


validatedForm : Form Model.CustomError Model.User
validatedForm =
    Form.update Model.validate Form.Validate initialForm
