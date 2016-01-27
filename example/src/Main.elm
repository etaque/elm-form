module Main where

import Task exposing (Task)

import StartApp
import Effects exposing (Effects)

import View exposing (view)
import Update exposing (init, update)


-- App

app = StartApp.start
  { init = init
  , update = update
  , view = view
  , inputs = [ ]
  }

main =
  app.html

port tasks : Signal (Task Effects.Never ())
port tasks =
  app.tasks


