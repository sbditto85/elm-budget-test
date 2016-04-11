module Budget.Account (view, init, update, Model, Action(..)) where

import Html exposing (div, span, text, button, Html, input)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Effects exposing (Effects)
import String exposing (toInt)
import Budget.Account.Common as Common
import Budget.Account.Types exposing (..)
import Budget.Account.PercentBase as AccountPercentBase
import Budget.Account.Fixed as AccountFixed
import Budget.Account.Percentage as AccountPercentage
--import Task exposing (..)


type Action =
      Recalculate Int Int
    | Update String String
    | UpdateNewName String
    | UpdateNewAmount String
    | EditMode


type alias Model = Budget.Account.Types.Model


init : String -> Int -> Int -> Int -> Int -> AccountType -> (Model, Effects Action)
init name amount baseAmount currentAmount factor accountType =
  let m = { name = name
           , amount = amount
           , newName = name
           , newAmount = (toString amount)
           , mode = NoEdit
           , error = ""
           , baseAmount = baseAmount
           , currentAmount = currentAmount
           , factor = factor
           , accountType = accountType
           }
      calcAmount = Common.calculate m
  in
    ({m | amount = calcAmount, newAmount = (toString calcAmount)}, Effects.none)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Recalculate baseAmount currentAmount ->
      let
        newModel = {model | baseAmount = baseAmount, currentAmount = currentAmount}
        amount = Common.calculate newModel
      in
        ({newModel | amount = amount}, Effects.none)

    Update name factor ->
      case toInt factor of
        Ok factor ->
          let
            amount = Common.calculate {model | factor = factor}
          in
            ({model | name = name
             , amount = amount
             , newName = name
             , newAmount = toString amount
             , mode = NoEdit
             , factor = factor
             , error = ""
             }, Effects.none)
        Err _ ->
          ({model | error = "Couldn't get a number for the amount."}, Effects.none)
    UpdateNewName newName ->
      ({model | newName = newName}, Effects.none)
    UpdateNewAmount newAmount ->
      ({model | newAmount = newAmount}, Effects.none)
    EditMode ->
      if canEdit model then
        ({model | mode = Edit}, Effects.none)
      else
        (model, Effects.none)


view : Address Action -> Model -> Html
view address model =
  let
    error = if model.error /= "" then [text model.error] else []
    editable = Common.canEdit model
    editButton = if editable then [button [onClick address (EditMode)] [text "Edit"]]  else []
    account =
      if model.mode == Edit then
        [ input [ value model.newName
                , placeholder "Account Name"
                , on "input" targetValue (\name -> message address (UpdateNewName name))
                ] []
        , input [ value model.newAmount
                , placeholder "Account Amount"
                , on "input" targetValue (\amount -> message address (UpdateNewAmount amount))
                ] []
        , button [onClick address (Update model.newName model.newAmount)] [text "Update"]
        ] ++ error
      else
        [ span [] [text model.name]
        , text " "
        , span [] [text (toString model.amount)]
        ] ++ editButton
  in
    div [] account


canEdit : Model -> Bool
canEdit model =
  case model.accountType of
    PercentBase -> AccountPercentBase.canEdit
    Fixed -> AccountFixed.canEdit
    Percentage -> AccountPercentage.canEdit
