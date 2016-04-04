module Budget.Group (view, init, update, Model, Action) where

import Budget.Account as BudgetAccount
import Budget.Account.Common as BudgetCommon
import Budget.Account.Types exposing (..)

import Html exposing (div, span, text, button, Html, input, select, option)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Effects exposing (Effects)
import String exposing (toInt)

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
  | Add String String String
  | Remove ID

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateNewName name ->
      ({model | newAccountName = name}, Effects.none)

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
      in
        ({model | accounts = List.map updateAccount model.accounts}, Effects.none)

    Add name amount accountType ->
      let mAccountType = BudgetCommon.stringToAccountType accountType
          rAmount = toInt amount
      in
        case (rAmount, mAccountType) of
          (Ok amnt, Just acctType) ->
            let newAccount = (model.nextID, fst <| BudgetAccount.init name amnt model.baseAmount model.currentAmount amnt acctType)
                newAccounts = model.accounts ++ [ newAccount ]
            in
              ({model |
                 accounts = newAccounts
               , nextID = model.nextID + 1
               , newAccountName = ""
               , newAccountFactor = ""
               , newAccountType = ""
               , error = ""
               }, Effects.none)
          _ ->
            ({model | error = "Error, could not add account"}, Effects.none)

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
                 ]
        ]
      )

accountsTotal : Model -> Int
accountsTotal model =
  List.sum (List.map (\(id, account) -> account.amount) model.accounts)

viewAccount : Address Action -> (ID, BudgetAccount.Model) -> Html
viewAccount address (id, model) =
  BudgetAccount.view (forwardTo address (UpdateAccount id)) model
