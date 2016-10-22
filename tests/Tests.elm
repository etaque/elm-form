module Tests exposing (..)

import Test exposing (..)
import Expect exposing (..)
import String
import Model
import Update
import Form exposing (Form)
import Form.Error exposing (..)


all : Test
all =
    describe "Initial example validation"
        [ test "has no output" <|
            \_ -> equal (Form.getOutput validatedForm) Nothing
        , test "has errors on expected fields" <|
            \_ ->
                equal
                    [ ( "email", InvalidString )
                    , ( "profile.age", InvalidInt )
                    , ( "profile.role", InvalidString )
                    , ( "profile.superpower", InvalidString )
                    ]
                    (Form.getErrors validatedForm)
        ]


initialForm : Form Model.CustomError Model.User
initialForm =
    Form.initial Model.initialFields Model.validate


validatedForm : Form Model.CustomError Model.User
validatedForm =
    Form.update Form.Validate initialForm
