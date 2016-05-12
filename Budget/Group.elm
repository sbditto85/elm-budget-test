module Budget.Group exposing (view, init, update, Model, Msg(..))

import Budget.Account as BudgetAccount
import Budget.Account.Common as BudgetCommon
import Budget.Account.Types exposing (..)
import Html exposing (div, span, text, button, Html, input, select, option)
import Html.App
import Html.Events exposing (on, targetValue, onInput, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Json.Decode as Json
-- import Signal exposing (message, Address, forwardTo)
-- import Cmd exposing (Cmd, task)
import String exposing (toInt)


--import Task exposing (succeed)


type alias ID =
  Int


type alias Model =
  { name : String
  , newAccountName : String
  , newAccountFactor : String
  , newAccountType : String
  , error : String
  , accounts : List ( ID, BudgetAccount.Model )
  , baseAmount : Int
  , currentAmount : Int
  , remainingAmount : Int
  , nextID : ID
  }


init : String -> Int -> Int -> ( Model, Cmd Msg )
init name baseAmount currentAmount =
  ( { name = name
    , accounts = []
    , newAccountName = ""
    , newAccountFactor = ""
    , newAccountType = ""
    , error = ""
    , nextID = 0
    , baseAmount =
        baseAmount
        -- TODO: be the amount to budget across all groups
    , currentAmount =
        currentAmount
        -- TODO: be the amount to budget for the group
    , remainingAmount =
        currentAmount
        -- TODO: be the amount left to budget
    }
  , Cmd.none
  )


type Msg
  = UpdateAccount ID BudgetAccount.Msg
  | UpdateNewName String
  | UpdateNewFactor String
  | UpdateNewAccountType String
  | UpdateCurrentAmount Int
  | Add String String String
  | Remove ID
  | Recalculate Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Recalculate baseAmount currentAmount ->
      ( model, Cmd.none )
    UpdateNewName name ->
      ( { model | newAccountName = name }, Cmd.none )

    UpdateCurrentAmount amount ->
      ( { model | currentAmount = amount }, Cmd.none )

    UpdateNewFactor amount ->
      ( { model | newAccountFactor = amount }, Cmd.none )

    UpdateNewAccountType accountType ->
      ( { model | newAccountType = accountType }, Cmd.none )

    UpdateAccount id budgetMsg ->
      let
        updateAccount ( accountId, account ) =
          if accountId == id then
            ( accountId, fst <| BudgetAccount.update budgetMsg account )
          else
            ( accountId, account )

        newModel =
          { model | accounts = List.map updateAccount model.accounts }

        recalculate ( accountId, account ) ( accounts, remainingAmount ) =
          case budgetMsg of
            BudgetAccount.Update _ _ ->
              let
                newAccount =
                  fst <| BudgetAccount.update (BudgetAccount.Recalculate newModel.baseAmount newModel.currentAmount) account
              in
                ( accounts ++ [ ( accountId, newAccount ) ], remainingAmount - newAccount.amount )

            _ ->
              ( newModel.accounts, newModel.remainingAmount )

        ( accounts, remainingAmount ) =
          List.foldl recalculate ( [], model.currentAmount ) newModel.accounts
      in
        ( { newModel | remainingAmount = remainingAmount, accounts = accounts }, Cmd.none )

    Add name amount accountType ->
      let
        mAccountType =
          BudgetCommon.stringToAccountType accountType

        rAmount =
          toInt amount
      in
        case ( rAmount, mAccountType ) of
          ( Ok amnt, Just acctType ) ->
            let
              newAccount =
                ( model.nextID, fst <| BudgetAccount.init name amnt model.baseAmount model.currentAmount amnt acctType )

              newAccounts =
                model.accounts ++ [ newAccount ]

              newModel =
                { model
                  | accounts = newAccounts
                  , nextID = model.nextID + 1
                  , newAccountName = ""
                  , newAccountFactor = ""
                  , newAccountType = ""
                  , error = ""
                }

              newRemainingAmount =
                model.remainingAmount - (BudgetCommon.calculate (snd newAccount))
            in
              ( { newModel
                  | remainingAmount = newRemainingAmount
                }
              , Cmd.none
              )

          -- task <| succeed (UpdateCurrentAmount 300))
          _ ->
            ( { model
                | error = "Error, could not add account"
              }
            , Cmd.none
            )

    Remove id ->
      let
        newAccounts =
          List.filter (\( accountId, _ ) -> accountId /= id) model.accounts
      in
        ( { model | accounts = newAccounts }, Cmd.none )


view : Model -> Html Msg
view model =
  let
    accounts =
      List.map viewAccount model.accounts

    errors =
      if model.error /= "" then
        [ text model.error ]
      else
        []
  in
    div
      [ class "test" ]
      ([ div
          []
          ([ text model.name
           , text " "
           , input
              [ onInput UpdateNewName
              , placeholder "Account Name"
              , value model.newAccountName
              ]
              []
           , input
              [ onInput UpdateNewFactor
              , placeholder "Account Factor"
              , value model.newAccountFactor
              ]
              []
           , select
              [ on "change" (Json.map UpdateNewAccountType targetValue)
              ,  value model.newAccountType
              ]
              [ option [ value "" ] [ text "Select Account Type" ]
              , option [ value (BudgetCommon.accountTypeToString PercentBase) ] [ text (BudgetCommon.accountTypeToString PercentBase) ]
              , option [ value (BudgetCommon.accountTypeToString Fixed) ] [ text (BudgetCommon.accountTypeToString Fixed) ]
              , option [ value (BudgetCommon.accountTypeToString Percentage) ] [ text (BudgetCommon.accountTypeToString Percentage) ]
              ]
           , button [ onClick (Add model.newAccountName model.newAccountFactor model.newAccountType) ] [ text "Add Account" ]
           ]
            ++ errors
          )
       ]
        ++ accounts
        ++ [ div
              []
              [ span [] [ text "Total: " ]
              , text (toString (accountsTotal model))
              , span [] [ text " Remaining: " ]
              , text (toString (model.remainingAmount))
              ]
           ]
      )


accountsTotal : Model -> Int
accountsTotal model =
  List.sum (List.map (\( id, account ) -> account.amount) model.accounts)


viewAccount : ( ID, BudgetAccount.Model ) -> Html Msg
viewAccount ( id, model ) =
  Html.App.map (UpdateAccount id) (BudgetAccount.view model)
