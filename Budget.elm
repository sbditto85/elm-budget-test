module Budget (view, init, update, Model, Action) where

import Html exposing (div, span, text, button, Html, input, select, option, hr)
import Html.Events exposing (on, targetValue, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
import Signal exposing (message, Address, forwardTo)
import Budget.Group as BudgetGroup
import Effects exposing (Effects)
import String exposing (toInt)

type alias ID = Int

type alias Model =
  { newGroupName : String
  , newBaseAmount : String
  , groups : List (ID, BudgetGroup.Model)
  , baseAmount : Int
  , currentAmount : Int
  , error : String
  , nextID : ID
  }

type Action =
    UpdateGroup ID BudgetGroup.Action
  | UpdateNewName String
  | UpdateNewBaseAmount String
  | SetBaseAmount String
  | Add String
  | Remove ID


init : (Model, Effects Action)
init =
  ({ newGroupName = ""
   , newBaseAmount = "0"
   , groups = []
   , baseAmount = 0
   , currentAmount = 0
   , error = ""
   , nextID = 0
   }
  , Effects.none)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    UpdateNewName str ->
      ({model | newGroupName = str}, Effects.none)

    Add name ->
      let newGroup = (model.nextID, fst <| BudgetGroup.init name model.baseAmount model.currentAmount)
          newGroups = model.groups ++ [ newGroup ]
          newModel = {model |
                        groups = newGroups
                     , nextID = model.nextID + 1
                     , newGroupName = ""
                     , error = ""
                     }
      in
        (newModel, Effects.none)

    Remove id ->
      let newGroups = List.filter (\(groupId, _) -> groupId /= id) model.groups
      in
        ({model | groups = newGroups }, Effects.none)

    UpdateGroup id groupAction ->
      let updateGroup (groupId, group) =
            if groupId == id then
              (groupId, fst <| BudgetGroup.update groupAction group)
            else
              (groupId, group)
          --TODO: after the update, go through each group and calc total then set the next groups current value based on prev current value - the previous groups total
      in
        ({model | groups = List.map updateGroup model.groups}, Effects.none)

    UpdateNewBaseAmount baseAmount ->
      ({model | newBaseAmount = baseAmount}, Effects.none)

    SetBaseAmount baseAmountStr ->
      case toInt baseAmountStr of
        Ok baseAmount ->
          ({model | baseAmount = baseAmount, currentAmount = baseAmount, error = ""}, Effects.none)
        _ ->
          ({model | error = "Couldn't update base amount"}, Effects.none)

view : Address Action -> Model -> Html
view address model =
  let groups = List.map (viewGroup address) model.groups
      error = if model.error /= "" then [hr [] [], text model.error] else []
  in
    div
      [id "main"]

      ([ input [ on "input" targetValue (\baseAmount -> message address (UpdateNewBaseAmount baseAmount))
               , placeholder "Base Amount"
               , value model.newBaseAmount
               ]
               []
      , button [ onClick address (SetBaseAmount model.newBaseAmount)
               ]
               [ text "Update Base Amount"
               ]
      ] ++ error ++
      [ hr [] []
      , input [ on "input" targetValue (\name -> message address (UpdateNewName name))
              , placeholder "New Group Name"
              , value model.newGroupName
              ]
              []
      , button [ onClick address (Add model.newGroupName)
               ]
               [ text "Add Group"
               ]
      ]
      ++ groups)

viewGroup : Address Action -> (ID, BudgetGroup.Model) -> Html
viewGroup address (id, model) =
  BudgetGroup.view (forwardTo address (UpdateGroup id)) model
