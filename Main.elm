module Main where

import StartApp exposing (start)
import Budget exposing (view, init, update, Action, Model)
import Html
import Task
import Effects exposing (Effects, Never)


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , update = update
    , view = view
    , inputs = []
    }

main : Signal Html.Html
main =
  app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks
