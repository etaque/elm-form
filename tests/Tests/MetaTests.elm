module Tests.MetaTests exposing (all)

import Expect exposing (Expectation)
import Form.Error exposing (ErrorValue)
import Form.Test.ValidationExpectation as ValidationExpectation
import Form.Test.Helpers
import Form.Validate as Validate exposing (Validation, string)
import Test exposing (..)


all : Test
all =
    describe "meta tests"
        [ test "expect success but get error" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation Validate.email
                            ( "This is definitely not an email address"
                            , ValidationExpectation.Valid
                            )

                    expected =
                        ValidationExpectation.Invalid Form.Error.InvalidEmail
                            |> Expect.equal ValidationExpectation.Valid
                in
                    actual
                        |> Expect.equal expected
        , test "expect success get success" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation Validate.email
                            ( "validemail@example.com"
                            , ValidationExpectation.Valid
                            )
                in
                    actual
                        |> Expect.equal Expect.pass
        , test "expected error" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation Validate.email
                            ( "This is definitely not an email address"
                            , ValidationExpectation.Invalid Form.Error.Empty
                            )

                    expected =
                        ValidationExpectation.Invalid Form.Error.InvalidEmail
                            |> Expect.equal (ValidationExpectation.Invalid Form.Error.Empty)
                in
                    actual
                        |> Expect.equal expected
        , test "different error than expected" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation Validate.email
                            ( "This is definitely not an email address"
                            , ValidationExpectation.Invalid Form.Error.InvalidEmail
                            )
                in
                    actual
                        |> Expect.equal Expect.pass
        , test "custom error" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation validateSuperpower
                            ( "This is not a superpower"
                            , ValidationExpectation.InvalidCustomError InvalidSuperpower
                            )
                in
                    actual
                        |> Expect.equal Expect.pass
        , test "expect custom error but get no error" <|
            \() ->
                let
                    actual =
                        Form.Test.Helpers.getValidationExpectation validateSuperpower
                            ( "flying"
                            , ValidationExpectation.InvalidCustomError InvalidSuperpower
                            )

                    expected =
                        Expect.equal
                            (ValidationExpectation.InvalidCustomError InvalidSuperpower)
                            (ValidationExpectation.ValidDecodesTo Flying)
                in
                    actual
                        |> Expect.equal expected
        ]


type Superpower
    = Flying
    | Invisible


type CustomError
    = InvalidSuperpower


validateSuperpower : Validation CustomError Superpower
validateSuperpower =
    Validate.customValidation
        string
        (\s ->
            case s of
                "flying" ->
                    Ok Flying

                "invisible" ->
                    Ok Invisible

                _ ->
                    Err (Validate.customError InvalidSuperpower)
        )
