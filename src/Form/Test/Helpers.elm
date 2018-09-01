module Form.Test.Helpers exposing (expectError, getValidationExpectation, run)

import Expect exposing (Expectation)
import Form.Error exposing (ErrorValue)
import Form.Field as Field
import Form.Test.ValidationExpectation exposing (..)
import Form.Tree
import Form.Validate as Validate exposing (Validation)


getValidationExpectation : Validation e a -> ( String, ValidationExpectation e a ) -> Expect.Expectation
getValidationExpectation validation ( stringToValidate, validationExpectation ) =
    case validationExpectation of
        Valid ->
            case run validation stringToValidate of
                Ok resultingString ->
                    Expect.pass

                Err formWithErrors ->
                    case Form.Tree.valuesWithPath formWithErrors of
                        [ ( "fieldKey", actualErrorValue ) ] ->
                            Invalid actualErrorValue
                                |> Expect.equal Valid

                        _ ->
                            Expect.fail "Unexpected form structure (this is probably a bug in this testing library)."

        Invalid expectedError ->
            expectError validation stringToValidate expectedError validationExpectation

        Form.Test.ValidationExpectation.ValidDecodesTo decodesToValue ->
            case run validation stringToValidate of
                Ok resultingString ->
                    Expect.equal decodesToValue resultingString

                Err formWithErrors ->
                    case Form.Tree.valuesWithPath formWithErrors of
                        [ ( "fieldKey", actualErrorValue ) ] ->
                            Invalid actualErrorValue
                                |> Expect.equal Valid

                        _ ->
                            Expect.fail "Unexpected form structure (this is probably a bug in this testing library)."

        Form.Test.ValidationExpectation.InvalidCustomError expectedCustomError ->
            expectError validation stringToValidate (Form.Error.CustomError expectedCustomError) validationExpectation


expectError : Validation e a -> String -> ErrorValue e -> ValidationExpectation e a -> Expectation
expectError validation stringToValidate expectedError validationExpectation =
    case run validation stringToValidate of
        Ok value ->
            Expect.equal validationExpectation (ValidDecodesTo value)

        Err formWithErrors ->
            case Form.Tree.valuesWithPath formWithErrors of
                [ ( "fieldKey", actualError ) ] ->
                    Expect.equal (Invalid expectedError) (Invalid actualError)

                _ ->
                    Expect.fail "Unexpected form structure (this is probably a bug in this testing library)."


run : Validation e a -> String -> Result (Form.Error.Error e) a
run validation stringToValidate =
    Field.group [ ( "fieldKey", Field.string stringToValidate ) ]
        |> Validate.field "fieldKey" validation
