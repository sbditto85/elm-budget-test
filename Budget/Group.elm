module Budget.Group (view, init, update, Model, Action) where

import Budget.Account as BudgetAccount
import Budget.Account.Common as BudgetCommon
import Budget.Account.Types exposing (..)

import Html exposing (div, span, text, button, Html, input, select, option)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Effects exposing (Effects, task)
import String exposing (toInt)
--import Task exposing (succeed)

type alias ID = Int

type alias Model =
  { name : String
  , newAccountName : String
  , newAccountFactor : String
  , newAccountType : String
  , error : String
  , accounts : List (ID, BudgetAccount.Model)
  , baseAmount : Int
  , currentAmount : Int
  , nextID : ID
  }

init : String -> Int -> Int ->  (Model, Effects Action)
init name baseAmount currentAmount =
  ({ name = name
   , accounts = []
   , newAccountName = ""
   , newAccountFactor = ""
   , newAccountType = ""
   , error = ""
   , nextID = 0
   , baseAmount = baseAmount
   , currentAmount = currentAmount
   }, Effects.none)

type Action =
    UpdateAccount ID BudgetAccount.Action
  | UpdateNewName String
  | UpdateNewFactor String
  | UpdateNewAccountType String
  | UpdateCurrentAmount Int
  | Add String String String
  | Remove ID

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateNewName name ->
      ({model | newAccountName = name}, Effects.none)

    UpdateCurrentAmount amount ->
      ({model | currentAmount = amount}, Effects.none)

    UpdateNewFactor amount ->
      ({model | newAccountFactor = amount}, Effects.none)

    UpdateNewAccountType accountType ->
      ({model | newAccountType = accountType}, Effects.none)

    UpdateAccount id budgetAction ->
      let updateAccount (accountId, account) =
            if accountId == id then
              (accountId, fst <| BudgetAccount.update budgetAction account)
            else
              (accountId, account)
          newModel = {model | accounts = List.map updateAccount model.accounts}
          recalculate (accountId, account) (accounts, currentAmount) =
            case budgetAction of
              BudgetAccount.Update _ _ ->
                let
                  newAccount = fst <| BudgetAccount.update (BudgetAccount.Recalculate newModel.baseAmount currentAmount) account
                in
                  (accounts ++ [(accountId, newAccount)], currentAmount - newAccount.amount)
              _ -> (newModel.accounts, newModel.currentAmount)
          (accounts, currentAmount) = List.foldl recalculate ([], model.baseAmount) newModel.accounts
      in
        ({newModel | currentAmount = currentAmount, accounts = accounts}, Effects.none)

    Add name amount accountType ->
      let mAccountType = BudgetCommon.stringToAccountType accountType
          rAmount = toInt amount
      in
        case (rAmount, mAccountType) of
          (Ok amnt, Just acctType) ->
            let newAccount = (model.nextID, fst <| BudgetAccount.init name amnt model.baseAmount model.currentAmount amnt acctType)
                newAccounts = model.accounts ++ [ newAccount ]
                newModel = {model |
                             accounts = newAccounts
                           , nextID = model.nextID + 1
                           , newAccountName = ""
                           , newAccountFactor = ""
                           , newAccountType = ""
                           , error = ""
                           }
                newCurrentAmount = model.currentAmount - (BudgetCommon.calculate (snd newAccount))
            in
              ({newModel |
                 currentAmount = newCurrentAmount
               }, Effects.none) -- task <| succeed (UpdateCurrentAmount 300))
          _ ->
            ({model |
               error = "Error, could not add account"
             }, Effects.none)

    Remove id ->
      let newAccounts = List.filter (\(accountId, _) -> accountId == id) model.accounts
      in
        ({model | accounts = newAccounts }, Effects.none)


view : Address Action -> Model -> Html
view address model =
  let accounts = List.map (viewAccount address) model.accounts
      errors = if model.error /= "" then [text model.error] else []
  in
    div
      [class "test"]
      ( [ div [] ([ text model.name
                  , text " "
                  , input [ on "input" targetValue (\name -> message address (UpdateNewName name))
                          , placeholder "Account Name"
                          , value model.newAccountName
                          ] []
                  , input [ on "input" targetValue (\amount -> message address (UpdateNewFactor amount))
                          , placeholder "Account Factor"
                          , value model.newAccountFactor
                          ] []
                  , select
                      [ on "change" targetValue (\accountType -> message address (UpdateNewAccountType accountType))
                      , value model.newAccountType
                      ]
                      [ option [value ""] [text "Select Account Type"]
                      , option [value (BudgetCommon.accountTypeToString Tithing)] [text (BudgetCommon.accountTypeToString Tithing)]
                      , option [value (BudgetCommon.accountTypeToString Fixed)] [text (BudgetCommon.accountTypeToString Fixed)]
                      , option [value (BudgetCommon.accountTypeToString Percentage)] [text (BudgetCommon.accountTypeToString Percentage)]
                      ]
                  , button [onClick address (Add model.newAccountName model.newAccountFactor model.newAccountType)] [text "Add Account"]
                  ] ++ errors)
        ] ++ accounts ++
        [ div [] [ span [] [text "Total: "]
                 , text (toString (accountsTotal model))
                 , span [] [text " Remaining: "]
                 , text (toString (model.currentAmount))
                 ]
        ]
      )

accountsTotal : Model -> Int
accountsTotal model =
  List.sum (List.map (\(id, account) -> account.amount) model.accounts)

viewAccount : Address Action -> (ID, BudgetAccount.Model) -> Html
viewAccount address (id, model) =
  BudgetAccount.view (forwardTo address (UpdateAccount id)) model
