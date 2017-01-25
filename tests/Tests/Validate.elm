module Tests.Validate exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Fuzz exposing (..)
import Form.Validate as Validate exposing (Validation)
import Form.Field
import Form.Error
import Form.Tree as Tree


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
        ]


run : Validation e a -> Result (Form.Error.Error e) a
run validation =
    Form.Field.group [] |> validation
