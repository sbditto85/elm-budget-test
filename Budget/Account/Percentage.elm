module Budget.Account.Percentage exposing (calculate, canEdit)

import Budget.Account.Types exposing (Model, AccountType, Mode)


calculate : Model -> Int
calculate model =
  floor <| (toFloat model.factor / 100) * (toFloat model.currentAmount)


canEdit : Bool
canEdit =
  True
