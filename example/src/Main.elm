module Main exposing (..)

import Browser
import Update exposing (init, update)
import View exposing (view)


-- App


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
