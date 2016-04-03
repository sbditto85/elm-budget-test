module Budget.Account.Tithing (view, init, update, Model, Action) where

import Budget.Account.Default as Default

type alias Model = Default.Model

type Action = Default.Action

init = Default.init

update = Default.update

view = Default.view
