module Budget.Account.Fixed exposing (calculate, canEdit)

import Budget.Account.Types exposing (Model, AccountType, Mode)


calculate : Model -> Int
calculate model =
  model.factor


canEdit : Bool
canEdit =
  True
