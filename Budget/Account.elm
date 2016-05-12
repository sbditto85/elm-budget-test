module Budget.Account exposing (view, init, update, Model, Msg(..))

import Html exposing (div, span, text, button, Html, input)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (class, id, placeholder, value)
-- import Signal exposing (message, Address, forwardTo)
-- import Cmd exposing (Cmd)
import String exposing (toInt)
import Budget.Account.Common as Common
import Budget.Account.Types exposing (..)
import Budget.Account.PercentBase as AccountPercentBase
import Budget.Account.Fixed as AccountFixed
import Budget.Account.Percentage as AccountPercentage


--import Task exposing (..)


type Msg
  = Recalculate Int Int
  | Update String String
  | UpdateNewName String
  | UpdateNewFactor String
  | EditMode


type alias Model =
  Budget.Account.Types.Model


init : String -> Int -> Int -> Int -> Int -> AccountType -> ( Model, Cmd Msg )
init name amount baseAmount currentAmount factor accountType =
  let
    m =
      { name = name
      , amount = amount
      , newName = name
      , newFactor = (toString factor)
      , mode = NoEdit
      , error = ""
      , baseAmount = baseAmount
      , currentAmount = currentAmount
      , factor = factor
      , accountType = accountType
      }

    calcAmount =
      Common.calculate m
  in
    ( { m | amount = calcAmount, newFactor = (toString factor) }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
  case action of
    Recalculate baseAmount currentAmount ->
      let
        newModel =
          { model | baseAmount = baseAmount, currentAmount = currentAmount }

        amount =
          Common.calculate newModel
      in
        ( { newModel | amount = amount }, Cmd.none )

    Update name factor ->
      case toInt factor of
        Ok factor ->
          let
            amount =
              Common.calculate { model | factor = factor }
          in
            ( { model
                | name = name
                , amount = amount
                , newName = name
                , newFactor = toString factor
                , mode = NoEdit
                , factor = factor
                , error = ""
              }
            , Cmd.none
            )

        Err _ ->
          ( { model | error = "Couldn't get a number for the amount." }, Cmd.none )

    UpdateNewName newName ->
      ( { model | newName = newName }, Cmd.none )

    UpdateNewFactor newFactor ->
      ( { model | newFactor = newFactor }, Cmd.none )

    EditMode ->
      if canEdit model then
        ( { model | mode = Edit }, Cmd.none )
      else
        ( model, Cmd.none )


view : Model -> Html Msg
view model =
  let
    error =
      if model.error /= "" then
        [ text model.error ]
      else
        []

    editable =
      Common.canEdit model

    editButton =
      if editable then
        [ button [ onClick EditMode ] [ text "Edit" ] ]
      else
        []

    account =
      if model.mode == Edit then
        [ input
            [ value model.newName
            , placeholder "Account Name"
            , onInput UpdateNewName
            ]
            []
        , input
            [ value model.newFactor
            , placeholder "Account Factor"
            , onInput UpdateNewFactor
            ]
            []
        , button [ onClick (Update model.newName model.newFactor) ] [ text "Update" ]
        ]
          ++ error
      else
        [ span [] [ text model.name ]
        , text " "
        , span [] [ text (toString model.amount) ]
        ]
          ++ editButton
  in
    div [] account


canEdit : Model -> Bool
canEdit model =
  case model.accountType of
    PercentBase ->
      AccountPercentBase.canEdit

    Fixed ->
      AccountFixed.canEdit

    Percentage ->
      AccountPercentage.canEdit
