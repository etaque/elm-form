module Tests.Validate exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Form.Validate as Validate exposing (Validation)
import Form.Field as Field
import Form.Error as Error
import Form.Tree as Tree
import Form.Input as Input
import Form


all : Test
all =
    describe "Validate"
        [ fuzz (list int) "Transforms a list of successes to a success of lists" <|
            \nums ->
                nums
                    |> List.map Validate.succeed
                    |> Validate.sequence
                    |> run
                    |> Expect.equal (Ok nums)
        , fuzz3 (list string) string string "Transforms a list with successes and failures into a failure list" <|
            \strings firstErr secondErr ->
                let
                    successes =
                        List.map
                            (\str -> Validate.succeed str |> Validate.field str)
                            strings

                    failure str =
                        Validate.fail (Validate.customError str)
                            |> Validate.field str

                    validations =
                        successes ++ ((failure firstErr :: successes) ++ (failure secondErr :: successes))
                in
                    validations
                        |> Validate.sequence
                        |> run
                        |> Expect.equal
                            (Tree.group
                                [ ( firstErr, Validate.customError firstErr )
                                , ( secondErr, Validate.customError secondErr )
                                ]
                                |> Err
                            )
        , test "Puts the errors at the correct indexes" <|
            \_ ->
                let
                    validate =
                        Validate.field "field_name"
                            (Validate.list
                                (Validate.string |> Validate.andThen (Validate.minLength 4))
                            )

                    initialForm =
                        Form.initial [ ( "field_name", Field.list [ (Field.value (Field.String "longer")), (Field.value (Field.String "not")), (Field.value (Field.String "longer")) ] ) ] validate
                in
                    Expect.equal
                        [ ( "field_name.1", Error.ShorterStringThan 4 ) ]
                        (Form.getErrors initialForm)
        , test "Gets index errors from error groups" <|
            \_ ->
                let
                    validate =
                        Validate.field "field_name"
                            (Validate.list
                                (Validate.string |> Validate.andThen (Validate.minLength 4))
                            )

                    initialForm =
                        Form.initial [ ( "field_name", Field.list [ (Field.value (Field.String "longer")), (Field.value (Field.String "not")), (Field.value (Field.String "longer")) ] ) ] validate

                    updatedForm =
                        initialForm
                            |> Form.update validate (Form.Input "field_name.0" Form.Text (Field.String "longer"))
                            |> Form.update validate (Form.Input "field_name.1" Form.Text (Field.String "not"))
                            |> Form.update validate (Form.Input "field_name.2" Form.Text (Field.String "longer"))

                    expectedField =
                        { path = "field_name.1"
                        , value = Just "not"
                        , error = Just (Error.ShorterStringThan 4)
                        , liveError = Nothing
                        , isDirty = False
                        , isChanged = False
                        , hasFocus = False
                        }
                in
                    Expect.equal
                        expectedField
                        (Form.getFieldAsString "field_name.1" initialForm)
        ]


run : Validation e a -> Result (Error.Error e) a
run validation =
    Field.group [] |> validation
