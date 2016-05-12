module Main exposing (..)

import Html.App as Html
import Budget exposing (view, init, update, Action, Model)
import Html
import Task
-- import Effects exposing (Effects, Never)

main =
  Html.program
      { init = init, update = update, view = view, subscriptions = \_ -> Sub.none}
