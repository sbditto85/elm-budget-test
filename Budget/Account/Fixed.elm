module Budget.Account.Fixed (calculate, canEdit) where

import Budget.Account.Types exposing (Model, AccountType, Mode)


calculate : Model -> Int
calculate model =
  model.factor


canEdit : Bool
canEdit =
  True
