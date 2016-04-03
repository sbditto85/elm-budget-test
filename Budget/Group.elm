module Budget.Group (view, init, update, Model, Action) where

import Budget.Account.Default as BudgetAccount

import Html exposing (div, span, text, button, Html, input)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Effects exposing (Effects)
import String exposing (toInt)

type alias ID = Int

type alias Model =
  { name : String
  , newAccountName : String
  , newAccountAmount : String
  , error : String
  , accounts : List (ID, BudgetAccount.Model)
  , nextID : ID
  }

init : String ->  (Model, Effects Action)
init name =
  ({ name = name
   , accounts = []
   , newAccountName = ""
   , newAccountAmount = ""
   , error = ""
   , nextID = 0
   }, Effects.none)

type Action =
    UpdateAccount ID BudgetAccount.Action
  | UpdateNewName String
  | UpdateNewAmount String
  | Add String String
  | Remove ID

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateNewName name ->
      ({model | newAccountName = name}, Effects.none)

    UpdateNewAmount amount ->
      ({model | newAccountAmount = amount}, Effects.none)

    UpdateAccount id budgetAction ->
      let updateAccount (accountId, account) =
            if accountId == id then
              (accountId, fst <| BudgetAccount.update budgetAction account)
            else
              (accountId, account)
      in
        ({model | accounts = List.map updateAccount model.accounts}, Effects.none)

    Add name amount ->
      case toInt amount of
        Ok amount ->
          let newAccount = (model.nextID, fst <| BudgetAccount.init name amount 1000 800 0)
              newAccounts = model.accounts ++ [ newAccount ]
          in
            ({model |
               accounts = newAccounts
             , nextID = model.nextID + 1
             , newAccountName = ""
             , newAccountAmount = ""
             , error = ""
             }, Effects.none)
        Err _ ->
          ({model | error = "Could not get a number from account value"}, Effects.none)

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
                  , input [ on "input" targetValue (\amount -> message address (UpdateNewAmount amount))
                          , placeholder "Account Amount"
                          , value model.newAccountAmount
                          ] []
                  , button [onClick address (Add model.newAccountName model.newAccountAmount)] [text "Add Account"]
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
