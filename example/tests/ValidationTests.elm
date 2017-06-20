module ValidationTests exposing (..)

import Form.Error
import Form.Test
import Form.Test.ValidationExpectation exposing (ValidationExpectation(..))
import Form.Validate
import Model
import Test exposing (..)


suite : Test
suite =
    describe "validations"
        [ Form.Test.describeValidation "superpower"
            Model.validateSuperpower
            [ ( "invisible", Valid )
            , ( "unvisible", Invalid (Form.Error.CustomError Model.InvalidSuperpower) )
            , ( "invisible", ValidDecodesTo Model.Invisible )
            , ( "flying", ValidDecodesTo Model.Flying )
            ]
        , Form.Test.describeValidation "email"
            Form.Validate.email
            [ ( "valid@email.com", Valid )
            , ( "This is definitely not an email address", Invalid Form.Error.InvalidEmail )
            , ( "stillvalid@withoutTLD", Valid )
            ]
        , Form.Test.describeValidation "naturalInt"
            Model.naturalInt
            [ ( "123", ValidDecodesTo 123 )
            , ( "-123", Invalid (Form.Error.CustomError Model.Nope) )
            ]
        ]
