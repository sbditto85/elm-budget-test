module Budget.Account.Common (accountTypeToString, stringToAccountType, calculate, canEdit) where

import Budget.Account.Types exposing (..)
import Budget.Account.PercentBase as PercentBase
import Budget.Account.Fixed as Fixed
import Budget.Account.Percentage as Percentage


accountTypeToString : AccountType -> String
accountTypeToString accountType =
  case accountType of
    PercentBase ->
      "PercentBase"

    Fixed ->
      "Fixed"

    Percentage ->
      "Percentage"


stringToAccountType : String -> Maybe AccountType
stringToAccountType str =
  case str of
    "PercentBase" ->
      Just PercentBase

    "Fixed" ->
      Just Fixed

    "Percentage" ->
      Just Percentage

    _ ->
      Nothing


calculate : Model -> Int
calculate model =
  case model.accountType of
    PercentBase ->
      PercentBase.calculate model

    Percentage ->
      Percentage.calculate model

    Fixed ->
      Fixed.calculate model


canEdit : Model -> Bool
canEdit model =
  case model.accountType of
    PercentBase ->
      PercentBase.canEdit

    Percentage ->
      Percentage.canEdit

    Fixed ->
      Fixed.canEdit
