module Budget exposing (view, init, update, Model, Msg)

import Html exposing (div, span, text, button, Html, input, select, option, hr)
import Html.App
import Html.Events exposing (on, targetValue, onInput, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
-- import Signal exposing (message, Address, forwardTo)
import Budget.Group as BudgetGroup
-- import Cmd exposing (Cmd)
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

type Msg =
    UpdateGroup ID BudgetGroup.Msg
  | UpdateNewName String
  | UpdateNewBaseAmount String
  | SetBaseAmount String
  | Add String
  | Remove ID


init : (Model, Cmd Msg)
init =
  ({ newGroupName = ""
   , newBaseAmount = "0"
   , groups = []
   , baseAmount = 0
   , currentAmount = 0
   , error = ""
   , nextID = 0
   }
  , Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    UpdateNewName str ->
      ({model | newGroupName = str}, Cmd.none)

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
        (newModel, Cmd.none)

    Remove id ->
      let newGroups = List.filter (\(groupId, _) -> groupId /= id) model.groups
      in
        ({model | groups = newGroups }, Cmd.none)

    UpdateGroup id groupMsg ->
      let updateGroup (groupId, group) =
            if groupId == id then
              (groupId, fst <| BudgetGroup.update groupMsg group)
            else
              (groupId, group)
          --TODO: after the update, go through each group and calc total then set the next groups current value based on prev current value - the previous groups total
          -- newModel =
      --       { model | groups = List.map updateGroup model.groups }

      --     recalculate ( groupId, group ) ( groups, remainingAmount ) =
      --       case groupMsg of
      --         BudgetGroup.UpdateAccount _ _ ->
      --           let
      --             newGroup =
      --               fst <| BudgetGroup.update (BudgetGroup.Recalculate newModel.baseAmount newModel.currentAmount) group
      --           in
      --             ( groups ++ [ ( groupId, newGroup ) ], remainingAmount - newGroup.amount )

      --         _ ->
      --           ( newModel.groups, newModel.remainingAmount )

      --     ( groups, remainingAmount ) =
      --       List.foldl recalculate ( [], model.currentAmount ) newModel.groups
      -- in
      --   ( { newModel | remainingAmount = remainingAmount, groups = groups }, Cmd.none )
      in
        ({model | groups = List.map updateGroup model.groups}, Cmd.none)

    UpdateNewBaseAmount baseAmount ->
      ({model | newBaseAmount = baseAmount}, Cmd.none)

    SetBaseAmount baseAmountStr ->
      case toInt baseAmountStr of
        Ok baseAmount ->
          ({model | baseAmount = baseAmount, currentAmount = baseAmount, error = ""}, Cmd.none)
        _ ->
          ({model | error = "Couldn't update base amount"}, Cmd.none)

view : Model -> Html Msg
view model =
  let groups = List.map viewGroup model.groups
      error = if model.error /= "" then [hr [] [], text model.error] else []
  in
    div
      [id "main"]

      ([ input [ onInput UpdateNewBaseAmount
               , placeholder "Base Amount"
               , value model.newBaseAmount
               ]
               []
      , button [ onClick (SetBaseAmount model.newBaseAmount)
               ]
               [ text "Update Base Amount"
               ]
      ] ++ error ++
      [ hr [] []
      , input [ onInput UpdateNewName
              , placeholder "New Group Name"
              , value model.newGroupName
              ]
              []
      , button [ onClick (Add model.newGroupName)
               ]
               [ text "Add Group"
               ]
      ]
      ++ groups)

viewGroup : (ID, BudgetGroup.Model) -> Html Msg
viewGroup (id, model) =
  Html.App.map (UpdateGroup id) (BudgetGroup.view  model)
