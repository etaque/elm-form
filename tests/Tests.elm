module Tests exposing (..)

import Test exposing (..)
import Tests.Example
import Tests.MetaTests
import Tests.Validate


all : Test
all =
    describe "elm-form Suite"
        [ Tests.Example.all
        , Tests.Validate.all
        , Tests.MetaTests.all
        ]
