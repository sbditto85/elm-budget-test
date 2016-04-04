module Budget.Account.Tithing (calculate, canEdit) where

import Budget.Account.Types exposing (Model, AccountType, Mode)

calculate : Model -> Int
calculate model =
  floor <| toFloat model.baseAmount * 0.10

canEdit : Bool
canEdit = False
