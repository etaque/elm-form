module Form.Test exposing (describeValidation, testValidation)

{-| Helpers to test your validations. See
[an example validation test suite here](https://github.com/etaque/elm-form/tree/master/example/tests).

@docs describeValidation, testValidation

-}

import Form.Test.Helpers as TestHelpers
import Form.Test.ValidationExpectation exposing (ValidationExpectation(..))
import Form.Validate as Validate exposing (Validation)
import Test exposing (..)


{-| Test your `Validation`s with a List of test input String, ValidationExpectation pairs.

    import Form.Error
    import Form.Test exposing (..)
    import Form.Test.ValidationExpectation exposing (ValidationExpectation(..))
    import Form.Validate

    describeValidation "email"
       Form.Validate.email
       [ ( "valid@email.com", Valid )
       , ( "This is definitely not an email address"
         , Invalid Form.Error.InvalidEmail
         )
       ]

-}
describeValidation : String -> Validate.Validation e a -> List ( String, ValidationExpectation e a ) -> Test
describeValidation description validation cases =
    let
        testCases =
            List.map (testValidation validation) cases
    in
    describe (description ++ " validations") testCases


{-| Create a single test case for a `Validation`.

    import Form.Error
    import Form.Test exposing (..)
    import Form.Test.ValidationExpectation exposing (ValidationExpectation(..))
    import Form.Validate

    testValidation Form.Validate.email ( "valid@email.com", Valid )

-}
testValidation : Validate.Validation e a -> ( String, ValidationExpectation e a ) -> Test
testValidation validation (( stringToValidate, validationExpectation ) as validationCase) =
    let
        shallowExpectationString =
            case validationExpectation of
                Valid ->
                    "Valid"

                ValidDecodesTo a ->
                    "ValidDecodesTo"

                Invalid _ ->
                    "Invalid "

                InvalidCustomError _ ->
                    "InvalidCustomError"
    in
    Test.test ("expect " ++ shallowExpectationString ++ "with input '" ++ stringToValidate ++ "'") <|
        \() ->
            TestHelpers.getValidationExpectation validation validationCase
