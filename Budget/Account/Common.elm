module Budget.Account.Common (accountTypeToString, stringToAccountType, calculate, canEdit) where

import Budget.Account.Types exposing (..)
import Budget.Account.Tithing as Tithing
import Budget.Account.Fixed as Fixed
import Budget.Account.Percentage as Percentage

accountTypeToString : AccountType -> String
accountTypeToString accountType =
  case accountType of
    Tithing ->
      "Tithing"
    Fixed ->
      "Fixed"
    Percentage ->
      "Percentage"

stringToAccountType : String -> Maybe AccountType
stringToAccountType str =
  case str of
    "Tithing" ->
      Just Tithing
    "Fixed" ->
      Just Fixed
    "Percentage" ->
      Just Percentage
    _ ->
      Nothing

calculate : Model -> Int
calculate model =
  case model.accountType of
    Tithing ->
      Tithing.calculate model
    Percentage ->
      Percentage.calculate model
    Fixed ->
      Fixed.calculate model

canEdit : Model -> Bool
canEdit model =
  case model.accountType of
    Tithing ->
      Tithing.canEdit
    Percentage ->
      Percentage.canEdit
    Fixed ->
      Fixed.canEdit
